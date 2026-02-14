extends Area2D
##
## Cell / Flower Creature with:
## - Per-instance gradient coloring + size
## - Simple wandering + social nudging
## - Mating + recombination with linked loci on 2 chromosomes
## - Hotspots + inversion that suppresses recombination in a segment
## - Drawn procedural flower petals (including heart petals) + edge styles
## - Optional genetics label overlay
##
## Copy/paste ready. ✅
##
## Scene assumptions (child nodes):
## - Sprite2D named "Sprite2D"
## - Timer   named "WanderTimer"
## - Label   named "SpeciesLabel"
##
## Notes:
## - We draw the flower procedurally in _draw(); the Sprite2D is kept hidden.
## - The Sprite2D can still hold a GradientTexture2D that we duplicate per instance
##   so each cell’s gradient can be unique without affecting others.
## - Spot patterns use stable randomness per instance (seeded RNG) so freckles don't flicker.
## - Godot 4 note: no nested functions inside functions; no “free-floating” code outside funcs.
## - Godot 4 note: avoid inline lambdas for signal callbacks (can trigger:
##   “Standalone lambdas cannot be accessed”). We connect timers to a real method.
##

@onready var sprite: Sprite2D = $Sprite2D
@onready var wander_timer: Timer = $WanderTimer
@onready var species_label: Label = $SpeciesLabel

signal cell_selected(cell)

# ------------------------------------------------------------
# Instance / UI state
# ------------------------------------------------------------
var species_id := 0
var glow_strength := 0.0
var selected := false
var _base_label_text := ""
var show_genetics := false

# Optional link back to a world controller (not used directly here)
var world: Node = null
func set_world(w: Node) -> void:
	world = w


# ============================================================
# CHROMOSOMES (2 total): linked loci, per-chromosome lam, hotspots, inversions
# ============================================================
##
## We treat genes as a dictionary. During recombination:
## - Genes on each chromosome are linked by position (0..1).
## - Crossovers occur with a Poisson(lam) number of cuts.
## - Hotspots/coldspots weight where cuts are likely.
## - An inversion can suppress crossovers in a span when parents differ.
##
## The result is “chunks” of traits staying together, rather than full independence.

const CHR1_LOCI := [
	# "Life / behavior / timing" bundle
	{ "name": "mut_rate",          "pos": 0.05 },
	{ "name": "lifespan",          "pos": 0.12 },
	{ "name": "speed",             "pos": 0.22 },
	{ "name": "mate_cooldown",     "pos": 0.25 },
	{ "name": "social",            "pos": 0.34 },
	{ "name": "social_strength",   "pos": 0.40 },
	{ "name": "wander_rate",       "pos": 0.55 },
	{ "name": "wander_keep",       "pos": 0.62 },
	{ "name": "fertile_start",     "pos": 0.82 },
	{ "name": "fertile_end",       "pos": 0.88 },
	{ "name": "species_threshold", "pos": 0.95 },
]

const CHR2_LOCI := [
	# "Plant weirdness / visuals / chemistry" bundle
	{ "name": "petal_count",     "pos": 0.08 },

	# Gradient genes (linked color chunks!)
	{ "name": "hue_a",           "pos": 0.10 },
	{ "name": "sat_a",           "pos": 0.12 },
	{ "name": "val_a",           "pos": 0.14 },
	{ "name": "hue_b",           "pos": 0.18 },
	{ "name": "sat_b",           "pos": 0.20 },
	{ "name": "val_b",           "pos": 0.24 },

	{ "name": "petal_length",    "pos": 0.16 },
	{ "name": "petal_width",     "pos": 0.22 },
	{ "name": "flower_size",     "pos": 0.30 },

	{ "name": "leaf_shape",      "pos": 0.46 },
	{ "name": "leaf_edge",       "pos": 0.52 },

	# Pattern / glow / toxic in a region that can be "locked" by an inversion
	{ "name": "spot_pattern",    "pos": 0.60 },
	{ "name": "vein_contrast",   "pos": 0.66 },

	{ "name": "glow_max",        "pos": 0.72 },
	{ "name": "glow_pulse_rate", "pos": 0.76 },
	{ "name": "glow_ramp",       "pos": 0.79 },

	{ "name": "toxic",           "pos": 0.86 },
	{ "name": "scent",           "pos": 0.92 },
]

# Per-chromosome recombination tuning
# lam = expected crossovers per meiosis per chromosome
#
# λ value    What happens
# 0.0        No recombination. Whole chromosome inherited from one parent. Pure lineages.
# 0.5        Usually 0–1 crossovers. Big trait chunks stay together.
# 1.0        About 1 crossover per chromosome. Moderate mixing.
# 2.0        2-ish crossovers. Smaller chunks. More scrambling.
# 5.0        Genetic confetti.
const CHR_CFG := {
	1: {
		"lam": 2.5,
		"hotspots": [
			{ "start": 0.50, "end": 0.70, "weight": 3.0 }, # breaks wander bundles sometimes
		],
		"coldspots": []
	},
	2: {
		"lam": 2.5,
		"hotspots": [
			{ "start": 0.65, "end": 0.82, "weight": 4.0 }, # "weirdness" recombines more often
		],
		"coldspots": []
	}
}

# Inversion that suppresses recombination in a segment (when heterozygous between parents)
# This "locks" pattern+glow+toxic so they travel together as a lineage chunk.
const INV_GLOW_LOCK := { "chr": 2, "start": 0.55, "end": 0.90 }


# ============================================================
# GENE SCHEMA
# ============================================================
##
## Defines how each gene is:
## - randomly initialized for founders (roll_random_genes)
## - mutated (mutate_value_by_schema)
## - clamped / validated
##
## Types:
## - float: continuous numeric within [min,max]
## - hue: wraps 0..1 (circular)
## - int: integer within [min,max]
## - bool: true/false
## - enum: one of listed values

const GENE_SCHEMA := {
	# HSV for gradient stop A
	"hue_a": { "type": "hue",   "min": 0.0, "max": 1.0, "mut_amount": 0.10 },
	"sat_a": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },
	"val_a": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },

	# HSV for gradient stop B
	"hue_b": { "type": "hue",   "min": 0.0, "max": 1.0, "mut_amount": 0.10 },
	"sat_b": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },
	"val_b": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },

	# Mutation rate controls overall mutation intensity for offspring
	"mut_rate": { "type": "float", "min": 0.0, "max": 0.35, "mut_amount": 0.08 },

	# Lifespan in seconds (age accumulates in _physics_process)
	"lifespan": { "type": "float", "min": 6.0, "max": 120.0, "mut_amount": 6.0 },

	# Movement
	"speed": { "type": "float", "min": 40.0, "max": 360.0, "mut_amount": 25.0 },

	# Social movement bias toward nearest neighbor
	"social": { "type": "float", "min": 0.0, "max": 2.0, "mut_amount": 0.25 },
	"social_strength": { "type": "float", "min": 0.0, "max": 0.5, "mut_amount": 0.05 },

	# Wander / direction change
	"wander_rate": { "type": "float", "min": 0.05, "max": 2.5, "mut_amount": 0.15 },
	"wander_keep": { "type": "float", "min": 0.0, "max": 1.0, "mut_amount": 0.10 },

	# Mating cooldown and fertility window (as fractions of lifespan)
	"mate_cooldown": { "type": "float", "min": 0.2, "max": 1.0, "mut_amount": 0.5 },
	"fertile_start": { "type": "float", "min": 0.0, "max": 0.2, "mut_amount": 0.08 },
	"fertile_end":   { "type": "float", "min": 0.6, "max": 1.0, "mut_amount": 0.08 },

	# Species threshold: max genetic distance allowed for mating
	"species_threshold": { "type": "float", "min": 0.4, "max": 0.9, "mut_amount": 0.08 },

	# Glow (fertility halo)
	"glow_max":        { "type": "float", "min": 1.0, "max": 2.0, "mut_amount": 0.25 },
	"glow_pulse_rate": { "type": "float", "min": 0.001, "max": 0.02, "mut_amount": 0.003 },
	"glow_ramp":       { "type": "float", "min": 0.5, "max": 15.0, "mut_amount": 2.0 },

	# Plant weirdness genes
	"petal_count":  { "type": "int",   "min": 3,   "max": 12,  "mut_amount": 2 },
	"petal_length": { "type": "float", "min": 0.5, "max": 3.0, "mut_amount": 0.15 },
	"petal_width":  { "type": "float", "min": 0.6, "max": 2.0, "mut_amount": 0.15 },
	"flower_size":  { "type": "float", "min": 0.5, "max": 3.0, "mut_amount": 0.20 },

	"leaf_shape": { "type": "enum", "values": ["round", "oval", "lance", "heart"], "mut_amount": 1 },
	"leaf_edge":  { "type": "enum", "values": ["smooth", "serrated", "lobed"],     "mut_amount": 1 },

	"spot_pattern":  { "type": "enum",  "values": ["none", "freckle", "polka", "ring"], "mut_amount": 1 },
	"vein_contrast": { "type": "float", "min": 0.0, "max": 1.0, "mut_amount": 0.18 },

	"toxic": { "type": "bool", "mut_amount": 1 },
	"scent": { "type": "enum", "values": ["none", "sweet", "funk", "sharp"], "mut_amount": 1 },

	# Structural variant: inversion lock. If parents differ, recombination suppressed in INV_GLOW_LOCK span.
	"inv_glow_lock": { "type": "bool", "mut_amount": 1 }
}

# Gene dictionary (all traits live here)
var genes := {}

# ------------------------------------------------------------
# Genetics label display controls
# ------------------------------------------------------------
func set_genetics_visible(on: bool) -> void:
	show_genetics = on
	refresh_label()

func refresh_label() -> void:
	if species_label == null:
		return

	if _base_label_text == "":
		_base_label_text = str(species_id % 1000)

	if not show_genetics:
		species_label.text = _base_label_text
		return

	if selected:
		species_label.text = _base_label_text + "\n" + gene_card_multiline()
	else:
		species_label.text = _base_label_text

func gene_card_multiline() -> String:
	var lines := []
	lines.append("petals: %d" % int(genes.get("petal_count", -1)))
	lines.append("petal L/W: %.2f / %.2f" % [float(genes.get("petal_length", 0.0)), float(genes.get("petal_width", 0.0))])
	lines.append("flower size: %.2f" % float(genes.get("flower_size", 1.0)))
	lines.append("leaf: %s / %s" % [str(genes.get("leaf_shape", "?")), str(genes.get("leaf_edge", "?"))])
	lines.append("pattern: %s | veins %.2f" % [str(genes.get("spot_pattern", "?")), float(genes.get("vein_contrast", 0.0))])
	lines.append("glow_max: %.2f" % float(genes.get("glow_max", 0.0)))
	lines.append("toxic: %s | scent: %s" % [str(bool(genes.get("toxic", false))), str(genes.get("scent", "none"))])
	lines.append("inv_glow_lock: %s" % str(bool(genes.get("inv_glow_lock", false))))
	return "\n".join(lines)

func gene_card_short() -> String:
	var pc = int(genes.get("petal_count", -1))
	var fs = float(genes.get("flower_size", 1.0))
	var pl = float(genes.get("petal_length", 0.0))
	var pw = float(genes.get("petal_width", 0.0))
	var leaf_shape_s = str(genes.get("leaf_shape", "?"))
	var leaf_edge_s = str(genes.get("leaf_edge", "?"))
	var spot = str(genes.get("spot_pattern", "?"))
	var veins = float(genes.get("vein_contrast", 0.0))
	var toxic_b = bool(genes.get("toxic", false))
	var scent_s = str(genes.get("scent", "none"))
	var inv = bool(genes.get("inv_glow_lock", false))
	var glow = float(genes.get("glow_max", 0.0))

	return "sp=%s petals=%d (L%.2f W%.2f) flower=%.2f leaf=%s/%s spots=%s veins=%.2f glow=%.2f toxic=%s scent=%s inv_lock=%s" % [
		species_tag(),
		pc, pl, pw, fs,
		leaf_shape_s, leaf_edge_s,
		spot, veins,
		glow,
		str(toxic_b),
		scent_s,
		str(inv)
	]


# ============================================================
# STATE
# ============================================================
var age := 0.0
var lifespan := 20.0
var dead := false

var velocity := Vector2.ZERO
var wander_dir := Vector2.RIGHT

var can_mate := true
var last_mate_time := -9999.0

var world_rect := Rect2()


# ============================================================
# READY
# ============================================================
func _ready() -> void:
	add_to_group("cells")
	world_rect = get_viewport_rect()

	# We draw the flower in _draw(), not via sprite art; keep sprite hidden.
	if sprite:
		sprite.visible = false

	queue_redraw()

	# Make gradient unique per instance (so changing colors doesn't affect all cells).
	# NOTE: sprite is hidden but still useful for storing per-instance gradient.
	if sprite and sprite.texture:
		var tex := sprite.texture as GradientTexture2D
		if tex != null:
			tex = tex.duplicate(true)
			if tex.gradient != null:
				tex.gradient = tex.gradient.duplicate(true)
			sprite.texture = tex

	# Founder roll if empty
	if genes.is_empty():
		genes = roll_random_genes()
	normalize_fertility_window(genes)

	lifespan = float(genes.get("lifespan", 20.0))

	apply_genes()

	# Wander timer rate driven by genes
	if wander_timer:
		wander_timer.wait_time = float(genes.get("wander_rate", 1.0))
		if not wander_timer.timeout.is_connected(_on_wander_timer_timeout):
			wander_timer.timeout.connect(_on_wander_timer_timeout)

	# Collision for mating
	if not area_entered.is_connected(_on_area_entered):
		area_entered.connect(_on_area_entered)

	_on_wander_timer_timeout()


# ============================================================
# GENE SYSTEM
# ============================================================
##
## Genetics overview:
## - genes is a Dictionary of values.
## - Founders: random values per schema
## - Reproduction:
##     1) recombine_linked on CHR1 and CHR2 using cutpoints
##     2) merge results to child
##     3) mutate each gene based on parent's blended mut_rate
##     4) normalize fertility window

func roll_random_genes() -> Dictionary:
	var g := {}
	for key in GENE_SCHEMA:
		var s = GENE_SCHEMA[key]
		match s["type"]:
			"float":
				g[key] = randf_range(float(s["min"]), float(s["max"]))
			"hue":
				g[key] = randf() # 0..1 wraps
			"int":
				g[key] = randi_range(int(s["min"]), int(s["max"]))
			"bool":
				g[key] = randf() < 0.5
			"enum":
				var vals: Array = s["values"]
				g[key] = vals[randi() % vals.size()]
			_:
				pass

	# Make sure inversion exists for founders
	if not g.has("inv_glow_lock"):
		g["inv_glow_lock"] = randf() < 0.25 # rare-ish structural variant
	return g

func normalize_fertility_window(g: Dictionary) -> void:
	var fs = float(g.get("fertile_start", 0.1))
	var fe = float(g.get("fertile_end", 0.8))

	fs = clamp(fs, 0.0, 0.95)
	fe = clamp(fe, 0.05, 1.0)

	if fs >= fe:
		fs = max(0.0, fe - 0.1)

	g["fertile_start"] = fs
	g["fertile_end"] = fe

func choose_parent_any(a, b):
	return a if randf() < 0.5 else b

func mutate_hue(h: float, mut_rate: float, amount: float) -> float:
	var out = h
	if randf() < mut_rate:
		out = fposmod(out + randf_range(-amount, amount), 1.0)
	return out

func hue_distance(a: float, b: float) -> float:
	var d = abs(a - b)
	return min(d, 1.0 - d)

func mutate_float(v: float, mut_rate: float, lo: float, hi: float, amount: float) -> float:
	if randf() < mut_rate:
		v += randf_range(-amount, amount)
	return clamp(v, lo, hi)

func mutate_int(v: int, mut_rate: float, lo: int, hi: int, step_amount: int) -> int:
	if randf() < mut_rate:
		# Mostly small steps, rarely a bigger leap
		var step = randi_range(-step_amount, step_amount)
		if randf() < (mut_rate * 0.15):
			step += randi_range(-step_amount * 2, step_amount * 2)
		v += step
	return clamp(v, lo, hi)

func mutate_bool(v: bool, mut_rate: float) -> bool:
	if randf() < mut_rate:
		return not v
	return v

func mutate_enum(v, mut_rate: float, values: Array):
	if randf() < mut_rate:
		return values[randi() % values.size()]
	return v

# --- Recombination utilities (linked loci + lam + hotspots + inversions) ---

## Poisson sampler used for number of crossovers per chromosome.
func poisson(lam: float) -> int:
	var L = exp(-lam)
	var k = 0
	var p = 1.0
	while p > L:
		k += 1
		p *= randf()
	return k - 1

## Applies hotspot/coldspot interval weighting.
## intervals: [ [a,b,w], ... ]
## mods: [ {start,end,weight}, ... ]
func apply_modifiers(intervals: Array, mods: Array) -> Array:
	var out := intervals
	for m in mods:
		var next := []
		for it in out:
			var a = float(it[0])
			var b = float(it[1])
			var w = float(it[2])
			var s = max(a, float(m["start"]))
			var e = min(b, float(m["end"]))

			if e <= s:
				next.append([a, b, w])
				continue

			if a < s:
				next.append([a, s, w])

			next.append([s, e, w * float(m["weight"])])

			if e < b:
				next.append([e, b, w])

		out = next
	return out

## Sample a crossover cut position in [0,1], respecting hotspot/coldspot weighting.
func sample_cut(cfg: Dictionary) -> float:
	var intervals := []
	intervals.append([0.0, 1.0, 1.0])

	if cfg.has("hotspots"):
		intervals = apply_modifiers(intervals, cfg["hotspots"])
	if cfg.has("coldspots"):
		intervals = apply_modifiers(intervals, cfg["coldspots"])

	var total := 0.0
	for it in intervals:
		total += (float(it[1]) - float(it[0])) * float(it[2])

	var r := randf() * total
	var acc := 0.0
	for it in intervals:
		var contrib = (float(it[1]) - float(it[0])) * float(it[2])
		acc += contrib
		if r <= acc:
			return lerp(float(it[0]), float(it[1]), randf())

	return randf()

func cut_is_suppressed(cut: float, suppressed_spans: Array) -> bool:
	for sp in suppressed_spans:
		if cut > float(sp[0]) and cut < float(sp[1]):
			return true
	return false

## Repeatedly samples cuts until one lands outside suppressed spans.
func sample_cut_respecting(cfg: Dictionary, suppressed_spans: Array, max_tries := 24) -> float:
	for i in range(max_tries):
		var c = sample_cut(cfg)
		if not cut_is_suppressed(c, suppressed_spans):
			return c
	return -1.0

## When parents differ in inversion state, crossovers inside the inversion are suppressed.
func get_suppressed_spans_for_chr2(a: Dictionary, b: Dictionary) -> Array:
	var spans := []
	var inv_a = bool(a.get("inv_glow_lock", false))
	var inv_b = bool(b.get("inv_glow_lock", false))
	if inv_a != inv_b:
		spans.append([float(INV_GLOW_LOCK["start"]), float(INV_GLOW_LOCK["end"])])
	return spans

## Produces ONE recombinant "chromatid" for the specified loci:
## - Generate N cuts (Poisson)
## - Walk loci in order and flip parent source at each cut
func recombine_linked(a: Dictionary, b: Dictionary, loci: Array, cfg: Dictionary, suppressed_spans: Array) -> Dictionary:
	var out := {}

	var lam = float(cfg.get("lam", 1.0))
	var n = poisson(lam)

	var cuts := []
	for i in range(n):
		var cut = sample_cut_respecting(cfg, suppressed_spans)
		if cut >= 0.0:
			cuts.append(cut)
	cuts.sort()

	var flip := false
	var ci := 0

	# Choose initial source randomly to avoid bias.
	if randf() < 0.5:
		flip = true

	for locus in loci:
		var pos := float(locus["pos"])
		var name := String(locus["name"])

		while ci < cuts.size() and pos > float(cuts[ci]):
			flip = !flip
			ci += 1

		# flip == false => take from a, flip == true => take from b
		out[name] = b.get(name) if flip else a.get(name)

	return out

func mutate_value_by_schema(key: String, value, cmut: float):
	if not GENE_SCHEMA.has(key):
		return value

	var s = GENE_SCHEMA[key]
	match s["type"]:
		"float":
			return mutate_float(float(value), cmut, float(s["min"]), float(s["max"]), float(s["mut_amount"]))
		"hue":
			return mutate_hue(float(value), cmut, float(s["mut_amount"]))
		"int":
			return mutate_int(int(value), cmut, int(s["min"]), int(s["max"]), int(s["mut_amount"]))
		"bool":
			return mutate_bool(bool(value), cmut)
		"enum":
			return mutate_enum(value, cmut, s["values"])
		_:
			return value

## Full child creation from two parents:
## - Recombinant chromosome 1 + chromosome 2 (with inversion suppression)
## - Merge gene dicts
## - Mutate all genes
## - Normalize fertility
func recombine_genes(a: Dictionary, b: Dictionary) -> Dictionary:
	var child := {}
	var cmut = lerp(float(a.get("mut_rate", 0.1)), float(b.get("mut_rate", 0.1)), randf())

	# Chromosome 1 (behavior)
	var chr1 = recombine_linked(a, b, CHR1_LOCI, CHR_CFG[1], [])

	# Chromosome 2 (visual/chemistry) with inversion suppression
	var suppressed2 = get_suppressed_spans_for_chr2(a, b)
	var chr2 = recombine_linked(a, b, CHR2_LOCI, CHR_CFG[2], suppressed2)

	# Merge into child
	for k in chr1.keys():
		child[k] = chr1[k]
	for k in chr2.keys():
		child[k] = chr2[k]

	# Any genes not covered by chromosomes still inherit independently.
	for key in GENE_SCHEMA:
		if child.has(key):
			continue
		var s = GENE_SCHEMA[key]
		match s["type"]:
			"float", "hue", "int", "bool", "enum":
				child[key] = choose_parent_any(a.get(key), b.get(key))
			_:
				pass

	# Mutate everything according to schema
	for key in child.keys():
		child[key] = mutate_value_by_schema(key, child[key], cmut)

	normalize_fertility_window(child)
	return child


# ============================================================
# APPLY GENES (visual + derived state)
# ============================================================
##
## Updates:
## - gradient texture colors
## - sprite scale (simple hook for flower_size)
## - species_id and label formatting
func apply_genes() -> void:
	if sprite and sprite.texture:
		var tex := sprite.texture as GradientTexture2D
		if tex != null and tex.gradient != null:
			var ca = Color.from_hsv(
				float(genes.get("hue_a", 0.0)),
				float(genes.get("sat_a", 1.0)),
				float(genes.get("val_a", 1.0)),
				1.0
			)
			var cb = Color.from_hsv(
				float(genes.get("hue_b", 0.0)),
				float(genes.get("sat_b", 1.0)),
				float(genes.get("val_b", 1.0)),
				1.0
			)
			tex.gradient.set_color(0, ca)
			tex.gradient.set_color(1, cb)

	# A safe visual hook for "flower_size" without needing new art
	if sprite:
		var fs = float(genes.get("flower_size", 1.0))
		sprite.scale = Vector2.ONE * clamp(fs, 0.5, 2.0)

	species_id = compute_species_id()

	if species_label != null:
		species_label.text = str(species_id % 1000)
		species_label.visible = true
		species_label.z_index = 100
		species_label.top_level = false
		species_label.position = Vector2(-30, -60)
		species_label.scale = Vector2(2, 2)

		_base_label_text = species_label.text
		refresh_label()

		# Tiny hint: toxic plants tint the label slightly
		var tox = bool(genes.get("toxic", false))
		species_label.modulate = Color(1, 0.9, 0.9, 1) if tox else Color(1, 1, 1, 1)

	queue_redraw()


# ============================================================
# DRAWING UTILITIES
# ============================================================

func quad_bezier(p0: Vector2, p1: Vector2, p2: Vector2, t: float) -> Vector2:
	var a = p0.lerp(p1, t)
	var b = p1.lerp(p2, t)
	return a.lerp(b, t)

## Standard petal outline via two quadratic beziers: base->tip left edge, then tip->base right edge.
func build_petal_outline(base: Vector2, tip: Vector2, left_ctrl: Vector2, right_ctrl: Vector2, steps: int) -> PackedVector2Array:
	var pts := PackedVector2Array()

	for i in range(steps + 1):
		var t = float(i) / float(steps)
		pts.append(quad_bezier(base, left_ctrl, tip, t))

	for i in range(steps, -1, -1):
		var t = float(i) / float(steps)
		pts.append(quad_bezier(base, right_ctrl, tip, t))

	return pts

## Edge style adds serration/lobes by perturbing points outward along the local normal.
## intensity 0..1
func apply_edge_style(points: PackedVector2Array, base: Vector2, tip: Vector2, edge: String, intensity: float) -> PackedVector2Array:
	if edge == "smooth" or points.size() < 6 or intensity <= 0.01:
		return points

	var axis = (tip - base)
	var axis_len = axis.length()
	if axis_len <= 0.001:
		return points

	axis = axis / axis_len
	var normal = axis.rotated(PI / 2.0)

	var out := PackedVector2Array()
	out.resize(points.size())

	for i in range(points.size()):
		var p = points[i]
		var t = float(i) / float(max(1, points.size() - 1)) # 0..1 around outline
		var side_weight = sin(PI * t) # 0 at ends, 1 mid (reduce distortion near base/tip)

		var jitter = 0.0
		match edge:
			"serrated":
				jitter = sin(t * TAU * 10.0) * 0.6
			"lobed":
				jitter = sin(t * TAU * 3.0) * 1.2
			_:
				jitter = 0.0

		var amp = intensity * side_weight * 3.0
		out[i] = p + normal * (jitter * amp)

	return out

## HEART PETAL OUTLINE (stable "petal-heart")
##
## Builds a heart-shaped petal silhouette oriented from `base` -> `tip`.
## - `width` controls lobe width.
## - `steps` controls smoothness (48–96 is great).
##
## Shape:
## - Notch near base (two corners + center notch)
## - Two lobes
## - Tapered tip
##
## IMPORTANT: no nested functions here (Godot 4).
func build_heart_outline(base: Vector2, tip: Vector2, width: float, steps: int) -> PackedVector2Array:
	steps = max(24, steps)

	var axis = (tip - base)
	var axis_len = axis.length()
	if axis_len <= 0.001:
		return PackedVector2Array()

	var forward = axis / axis_len
	var right = forward.rotated(PI / 2.0)

	# Shape tuning
	var notch_depth = axis_len * 0.18
	var lobe_height = axis_len * 0.35
	var tip_taper   = 0.55
	width = max(width, axis_len * 0.05)

	var pts := PackedVector2Array()

	var notch = base + forward * (notch_depth * 0.55)

	# Note: using +right for one side and -right for the other is fine,
	# because the path is just a polygon outline.
	var notch_left  = base + forward * (notch_depth * 0.25) + right * (width * 0.20)
	var notch_right = base + forward * (notch_depth * 0.25) - right * (width * 0.20)

	var lobe_left  = base + forward * lobe_height + right * (width * 0.55)
	var lobe_right = base + forward * lobe_height - right * (width * 0.55)

	var tip_point = tip

	# notch -> lobe_left
	var seg = max(4, steps / 6)
	for i in range(seg + 1):
		var t = float(i) / float(seg)
		pts.append(quad_bezier(notch, notch_left, lobe_left, t))

	# lobe_left -> tip with taper
	var seg2 = max(6, steps / 3)
	for i in range(seg2 + 1):
		var t = float(i) / float(seg2)
		var ctrl = lobe_left.lerp(base + forward * (axis_len * 0.75), 0.55)
		var p = quad_bezier(lobe_left, ctrl, tip_point, t)

		var along = (p - base).dot(forward)
		var u = clamp(along / axis_len, 0.0, 1.0)
		var squeeze = lerp(1.0, tip_taper, pow(u, 1.8))
		var lateral = (p - base).dot(right)
		p = base + forward * along + right * (lateral * squeeze)

		pts.append(p)

	# tip -> lobe_right (reverse) with taper
	for i in range(seg2, -1, -1):
		var t = float(i) / float(seg2)
		var ctrl = lobe_right.lerp(base + forward * (axis_len * 0.75), 0.55)
		var p = quad_bezier(lobe_right, ctrl, tip_point, t)

		var along = (p - base).dot(forward)
		var u = clamp(along / axis_len, 0.0, 1.0)
		var squeeze = lerp(1.0, tip_taper, pow(u, 1.8))
		var lateral = (p - base).dot(right)
		p = base + forward * along + right * (lateral * squeeze)

		pts.append(p)

	# lobe_right -> notch
	for i in range(seg, -1, -1):
		var t = float(i) / float(seg)
		pts.append(quad_bezier(notch, notch_right, lobe_right, t))

	# Ensure closure
	if pts.size() > 0 and pts[pts.size() - 1].distance_to(notch) > 0.001:
		pts.append(notch)

	return pts


# ============================================================
# DRAW
# ============================================================
##
## Procedural flower rendering:
## - uses genes for count/size/shape/edge/style
## - glow halo drawn as layered circles
## - spots drawn with stable per-instance RNG so they don't flicker

func _draw() -> void:
	if genes.is_empty():
		return

	# Stable randomness per instance so freckles don't flicker every frame
	var rng := RandomNumberGenerator.new()
	rng.seed = int(get_instance_id())

	var petal_count = int(genes.get("petal_count", 6))
	var petal_length = float(genes.get("petal_length", 0.8))
	var petal_width = float(genes.get("petal_width", 0.6))
	var flower_size = float(genes.get("flower_size", 1.0))

	var leaf_shape = str(genes.get("leaf_shape", "oval"))
	var leaf_edge  = str(genes.get("leaf_edge", "smooth"))

	var spot_pattern = str(genes.get("spot_pattern", "none"))
	var vein_contrast = float(genes.get("vein_contrast", 0.0))

	# Base color from gradient A
	var base_color = Color.from_hsv(
		float(genes.get("hue_a", 0.0)),
		float(genes.get("sat_a", 1.0)),
		float(genes.get("val_a", 1.0))
	)

	var center_color = Color.from_hsv(
		float(genes.get("hue_b", 0.0)),
		float(genes.get("sat_b", 1.0)),
		float(genes.get("val_b", 1.0))
	)

	var radius = 20.0 * flower_size
	var center = Vector2.ZERO

	# --- Halo (soft glow) ---
	# Glow is strongest when:
	# - can_mate == true
	# - age is inside fertility window
	# - not dead
	var fertile_start = float(genes.get("fertile_start", 0.1))
	var fertile_end = float(genes.get("fertile_end", 0.8))
	var fertile = age >= lifespan * fertile_start and age <= lifespan * fertile_end

	var pulse = 0.5 + 0.5 * sin(Time.get_ticks_msec() * float(genes.get("glow_pulse_rate", 0.01)))
	var glow_intensity = glow_strength * pulse
	var glow_color = Color.from_hsv(float(genes.get("hue_a", 0.0)), 0.6, 1.0, 1.0)

	if glow_intensity > 0.01:
		var a = clamp(glow_intensity * 0.35, 0.0, 0.45)
		draw_circle(center, radius * 1.25, Color(glow_color.r, glow_color.g, glow_color.b, a * 0.35))
		draw_circle(center, radius * 1.10, Color(glow_color.r, glow_color.g, glow_color.b, a * 0.55))
		draw_circle(center, radius * 0.95, Color(glow_color.r, glow_color.g, glow_color.b, a * 0.80))

	# --- Petals ---
	var steps = 12

	for i in range(petal_count):
		var angle = TAU * float(i) / float(petal_count)
		var dir = Vector2.RIGHT.rotated(angle)
		var side = dir.rotated(PI / 2.0)

		# Base radius/size
		var base_in: float = 0.20
		var length_mult: float = float(petal_length)
		var width_mult: float = float(petal_width)

		# leaf_shape controls silhouette
		match leaf_shape:
			"round":
				length_mult *= 0.75
				width_mult *= 1.25
				base_in = 0.24
			"oval":
				length_mult *= 0.95
				width_mult *= 1.05
				base_in = 0.22
			"lance":
				length_mult *= 1.25
				width_mult *= 0.75
				base_in = 0.18
			"heart":
				length_mult *= 0.95
				width_mult *= 1.20
				base_in = 0.24

		var base = center + dir * (radius * base_in)
		var tip = center + dir * (radius * length_mult)

		var w = radius * width_mult * 0.55
		w = clamp(w, radius * 0.10, radius * 0.95)

		# Control points for standard petals
		var forward_amt = radius * length_mult * 0.30
		var base_push = radius * 0.10
		var left_ctrl = base + dir * (forward_amt + base_push) + side * w
		var right_ctrl = base + dir * (forward_amt + base_push) - side * w

		var pts: PackedVector2Array
		if leaf_shape == "heart":
			var heart_w = clamp(w * 1.15, radius * 0.12, radius * 0.95)
			pts = build_heart_outline(base, tip, heart_w, 64) # smoother heart
		else:
			pts = build_petal_outline(base, tip, left_ctrl, right_ctrl, steps)

		# leaf_edge modifies outline: serrated/lobed
		var edge_intensity = clamp(vein_contrast, 0.0, 1.0)
		pts = apply_edge_style(pts, base, tip, leaf_edge, edge_intensity)

		draw_colored_polygon(pts, base_color)

		# Veins (stronger when vein_contrast is high)
		if vein_contrast > 0.05:
			var vein_color = base_color.lerp(Color.WHITE, vein_contrast)
			draw_line(base, tip, vein_color, 1.2 + 1.2 * vein_contrast)

			# Extra: heart gets two subtle side veins
			if leaf_shape == "heart" and vein_contrast > 0.15:
				var vein_color2 = base_color.lerp(Color.WHITE, vein_contrast * 0.8)
				draw_line(base, base + (tip - base) * 0.75 + side * (w * 0.25), vein_color2, 1.0)
				draw_line(base, base + (tip - base) * 0.75 - side * (w * 0.25), vein_color2, 1.0)

	# --- Center disk ---
	draw_circle(center, radius * 0.35, center_color)

	# --- Spot patterns ---
	match spot_pattern:
		"freckle":
			for j in range(8):
				var r = rng.randf_range(radius * 0.2, radius * 0.8)
				var a = rng.randf() * TAU
				draw_circle(center + Vector2.RIGHT.rotated(a) * r, 2.0, center_color)
		"polka":
			for j in range(petal_count):
				var a = TAU * float(j) / float(petal_count)
				var r = radius * 0.65
				draw_circle(center + Vector2.RIGHT.rotated(a) * r, 3.0, center_color)
		"ring":
			draw_circle(center, radius * 0.75, center_color)
		_:
			pass


# ============================================================
# INPUT (selection)
# ============================================================
func _input_event(viewport: Viewport, event: InputEvent, shape_idx: int) -> void:
	if event is InputEventMouseButton and event.button_index == MOUSE_BUTTON_LEFT and event.pressed:
		emit_signal("cell_selected", self)


# ============================================================
# SPECIES
# ============================================================
##
## Species ID:
## - quantizes hue/sat/val into bins
## - hashes them into an int
##
## Species distance:
## - hue distance dominates
## - sat/val contribute lightly

func compute_species_id() -> int:
	var qh = 12.0
	var qs = 6.0
	var qv = 6.0

	var ha = int(float(genes.get("hue_a", 0.0)) * qh)
	var sa = int(float(genes.get("sat_a", 1.0)) * qs)
	var va = int(float(genes.get("val_a", 1.0)) * qv)

	var hb = int(float(genes.get("hue_b", 0.0)) * qh)
	var sb = int(float(genes.get("sat_b", 1.0)) * qs)
	var vb = int(float(genes.get("val_b", 1.0)) * qv)

	return (ha << 0) ^ (sa << 4) ^ (va << 8) ^ (hb << 12) ^ (sb << 16) ^ (vb << 20)

func species_distance(a: Dictionary, b: Dictionary) -> float:
	var d_ha = hue_distance(float(a.get("hue_a", 0.0)), float(b.get("hue_a", 0.0)))
	var d_hb = hue_distance(float(a.get("hue_b", 0.0)), float(b.get("hue_b", 0.0)))

	var d_sa = abs(float(a.get("sat_a", 0.0)) - float(b.get("sat_a", 0.0)))
	var d_va = abs(float(a.get("val_a", 0.0)) - float(b.get("val_a", 0.0)))
	var d_sb = abs(float(a.get("sat_b", 0.0)) - float(b.get("sat_b", 0.0)))
	var d_vb = abs(float(a.get("val_b", 0.0)) - float(b.get("val_b", 0.0)))

	return (d_ha + d_hb) * 0.45 + (d_sa + d_va + d_sb + d_vb) * 0.10


# ============================================================
# MOVEMENT
# ============================================================
##
## Behavior:
## - wander_dir changes on timer
## - keep_in_bounds bounces off viewport edges
## - nudge_toward_others biases wander_dir toward nearest cell based on social genes

func _physics_process(delta: float) -> void:
	if dead:
		return

	age += delta
	if age >= lifespan:
		die()
		return

	velocity = wander_dir * float(genes.get("speed", 100.0))
	position += velocity * delta

	keep_in_bounds()
	update_glow(delta)

	queue_redraw()

func _on_wander_timer_timeout() -> void:
	var jitter = Vector2(randf_range(-1.0, 1.0), randf_range(-1.0, 1.0)).normalized()
	var keep = float(genes.get("wander_keep", 0.5))
	wander_dir = (wander_dir * keep + jitter * (1.0 - keep)).normalized()
	nudge_toward_others()

func nudge_toward_others() -> void:
	var others = get_tree().get_nodes_in_group("cells")
	var nearest = null
	var nearest_d2 = INF

	for o in others:
		if o == self:
			continue
		var d2 = position.distance_squared_to(o.position)
		if d2 < nearest_d2:
			nearest_d2 = d2
			nearest = o

	if nearest != null:
		var to_other = (nearest.position - position).normalized()
		var s = float(genes.get("social", 0.0)) * float(genes.get("social_strength", 0.0))
		wander_dir = (wander_dir * (1.0 - s) + to_other * s).normalized()


# ============================================================
# GLOW
# ============================================================
##
## Glow is computed each frame:
## - target is glow_max when fertile + can_mate + alive
## - glow_strength lerps toward target with glow_ramp
## - pulsing uses sin(Time * glow_pulse_rate)
## - self_modulate shifts toward glow color

func update_glow(delta: float) -> void:
	var fertile_start = float(genes.get("fertile_start", 0.1))
	var fertile_end = float(genes.get("fertile_end", 0.8))
	var fertile = age >= lifespan * fertile_start and age <= lifespan * fertile_end

	var target = float(genes.get("glow_max", 0.0)) if can_mate and fertile and not dead else 0.0
	glow_strength = lerp(glow_strength, target, float(genes.get("glow_ramp", 5.0)) * delta)

	var pulse = 0.5 + 0.5 * sin(Time.get_ticks_msec() * float(genes.get("glow_pulse_rate", 0.01)))
	var intensity = glow_strength * pulse

	var glow_color = Color.from_hsv(float(genes.get("hue_a", 0.0)), 0.6, 1.0, 1.0)
	self_modulate = Color(1, 1, 1, 1).lerp(glow_color, intensity)


# ============================================================
# MATING
# ============================================================
##
## Mating trigger:
## - uses Area2D collision (area_entered)
## - only one side initiates to avoid duplicates
## - checks:
##     dead
##     cooldown for self and other
##     fertility window (age fraction)
##     species distance <= species_threshold
## - spawns a baby by duplicating this node, then assigning child genes
##
## Cooldown reset:
## - uses a Timer from SceneTree
## - connects timeout to a real method (no inline lambda)

func _on_area_entered(other: Area2D) -> void:
	if other == self or not other.is_in_group("cells"):
		return

	# Only one side initiates to avoid duplicate events
	if get_instance_id() > other.get_instance_id():
		return

	if dead:
		return
	if not can_mate:
		return
	if "can_mate" in other and not other.can_mate:
		return

	var dist = species_distance(genes, other.genes)

	var fertile_start = float(genes.get("fertile_start", 0.1))
	var fertile_end = float(genes.get("fertile_end", 0.8))

	if age < lifespan * fertile_start:
		return
	if age > lifespan * fertile_end:
		return

	var my_thresh = float(genes.get("species_threshold", 0.5))
	if dist > my_thresh:
		return

	call_deferred("mate_with_deferred", other, dist)

func mate_with_deferred(partner: Area2D, dist: float) -> void:
	if dead:
		return
	if not is_instance_valid(partner):
		return
	if "dead" in partner and partner.dead:
		return

	mate_with(partner, dist)

func mate_with(partner: Area2D, dist: float = -1.0) -> void:
	if not is_instance_valid(partner):
		return

	can_mate = false
	partner.can_mate = false

	var child_genes = recombine_genes(genes, partner.genes)

	var baby = duplicate()
	baby.position = (position + partner.position) * 0.5 + Vector2(randf_range(-20.0, 20.0), randf_range(-20.0, 20.0))
	get_parent().add_child(baby)

	baby.genes = child_genes
	baby.apply_genes()
	baby.can_mate = false

	var cooldown = float(genes.get("mate_cooldown", 1.5))

	var self_ref: WeakRef = weakref(self)
	var partner_ref: WeakRef = weakref(partner)

	var t: SceneTreeTimer = get_tree().create_timer(cooldown)
	t.timeout.connect(Callable(self, "_on_mate_cooldown_timeout").bind(self_ref, partner_ref))

func _on_mate_cooldown_timeout(self_ref: WeakRef, partner_ref: WeakRef) -> void:
	var s = self_ref.get_ref()
	if s != null and is_instance_valid(s) and not s.dead:
		s.can_mate = true

	var p = partner_ref.get_ref()
	if p != null and is_instance_valid(p) and not p.dead:
		p.can_mate = true


# ============================================================
# DEATH
# ============================================================
##
## On death:
## - stop movement and timer
## - fade out sprite alpha (even though sprite is hidden, safe)
## - queue_free after tween

func die() -> void:
	dead = true
	can_mate = false
	set_physics_process(false)
	if wander_timer:
		wander_timer.stop()

	var tween = create_tween()
	if sprite:
		tween.tween_property(sprite, "modulate:a", 0.0, 0.6)
	tween.finished.connect(queue_free)


# ============================================================
# BOUNDS
# ============================================================
##
## Bounces inside viewport rectangle.
func keep_in_bounds() -> void:
	if position.x < world_rect.position.x:
		position.x = world_rect.position.x
		wander_dir.x = abs(wander_dir.x)
	elif position.x > world_rect.end.x:
		position.x = world_rect.end.x
		wander_dir.x = -abs(wander_dir.x)

	if position.y < world_rect.position.y:
		position.y = world_rect.position.y
		wander_dir.y = abs(wander_dir.y)
	elif position.y > world_rect.end.y:
		position.y = world_rect.end.y
		wander_dir.y = -abs(wander_dir.y)


# ============================================================
# ID HELPERS (optional)
# ============================================================
func short_id() -> String:
	return str(get_instance_id() % 100000)

func species_tag() -> String:
	return str(species_id % 1000)

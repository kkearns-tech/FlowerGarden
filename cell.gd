extends Area2D
##
## cell.gd (DIPLOID edition) 🧬🌸
##
## A procedural “flower creature” that:
## - Stores a *diploid genome* (two haplotypes per chromosome)
## - Produces gametes via recombination between its own haplotypes (meiosis)
## - Makes diploid offspring (one gamete from each parent)
## - Expresses phenotype (the old `genes` dictionary) from diploid alleles
## - Draws a procedural flower in _draw() (petals, heart petals, edges, spots, veins)
## - Shows optional genetics readout in the label
## - Updates a Capsule collision shape to match flower size + petals
##
## ------------------------------------------------------------
## REQUIRED CHILD NODES (by name)
## ------------------------------------------------------------
## - Sprite2D        : "Sprite2D"       (can be hidden; used for optional GradientTexture2D duplication)
## - Timer           : "WanderTimer"
## - Label           : "SpeciesLabel"
## - CollisionShape2D: "CollisionShape2D"   (Shape should be CapsuleShape2D)
##
## ------------------------------------------------------------
## ENUM EXPRESSION RULE (DOCUMENTED)
## ------------------------------------------------------------
## Diploid expression needs a rule for enums like leaf_shape/spot_pattern/scent:
##
## We use a simple “dominance by ordering” model:
## - The *order* of values in GENE_SCHEMA["<gene>"]["values"] is the dominance order.
## - Lower index = more dominant.
## - Example:
##   leaf_shape values = ["round","oval","lance","heart"]
##   If alleles are ("heart","oval"), expressed becomes "round"? No:
##   expressed becomes whichever has *lower index* among the two:
##     "oval" (idx 1) dominates "heart" (idx 3)
##
## This is easy to reason about and gives stable inheritance.
## Later you can replace this with per-gene dominance maps.
##

@onready var sprite: Sprite2D = $Sprite2D
@onready var wander_timer: Timer = $WanderTimer
@onready var species_label: Label = $SpeciesLabel
@onready var collision_shape: CollisionShape2D = $CollisionShape2D

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
# Loci positions are 0..1 along each chromosome. Nearby loci tend to inherit together.

const CHR1_LOCI := [
	# "Life / behavior / timing" bundle
	{ "name": "mut_rate", "pos": 0.05 },
	{ "name": "lifespan", "pos": 0.12 },
	{ "name": "speed", "pos": 0.22 },
	{ "name": "mate_cooldown", "pos": 0.25 },
	{ "name": "social", "pos": 0.34 },
	{ "name": "social_strength", "pos": 0.40 },
	{ "name": "wander_rate", "pos": 0.55 },
	{ "name": "wander_keep", "pos": 0.62 },
	{ "name": "fertile_start", "pos": 0.82 },
	{ "name": "fertile_end", "pos": 0.88 },
	{ "name": "species_threshold", "pos": 0.95 },
]

const CHR2_LOCI := [
	# "Plant weirdness / visuals / chemistry" bundle
	{ "name": "petal_count", "pos": 0.08 },

	# Linked color chunk
	{ "name": "hue_a", "pos": 0.10 },
	{ "name": "sat_a", "pos": 0.12 },
	{ "name": "val_a", "pos": 0.14 },
	{ "name": "hue_b", "pos": 0.18 },
	{ "name": "sat_b", "pos": 0.20 },
	{ "name": "val_b", "pos": 0.24 },

	{ "name": "petal_length", "pos": 0.16 },
	{ "name": "petal_width", "pos": 0.22 },
	{ "name": "flower_size", "pos": 0.30 },

	{ "name": "leaf_shape", "pos": 0.46 },
	{ "name": "leaf_edge", "pos": 0.52 },

	# Region that can be "locked" by inversion
	{ "name": "spot_pattern", "pos": 0.60 },
	{ "name": "vein_contrast", "pos": 0.66 },
	{ "name": "glow_max", "pos": 0.72 },
	{ "name": "glow_pulse_rate", "pos": 0.76 },
	{ "name": "glow_ramp", "pos": 0.79 },
	{ "name": "toxic", "pos": 0.86 },
	{ "name": "scent", "pos": 0.92 },

	# Structural variant allele (stored on chr2)
	{ "name": "inv_glow_lock", "pos": 0.94 },
]

# lam = expected crossovers per meiosis per chromosome
const CHR_CFG := {
	1: {
		"lam": 2.5,
		"hotspots": [
			{ "start": 0.50, "end": 0.70, "weight": 3.0 },
		],
		"coldspots": []
	},
	2: {
		"lam": 2.5,
		"hotspots": [
			{ "start": 0.65, "end": 0.82, "weight": 4.0 },
		],
		"coldspots": []
	}
}

# Inversion span: if a parent is heterozygous for inv_glow_lock,
# crossovers inside this region are suppressed for that parent's meiosis.
const INV_GLOW_LOCK := { "chr": 2, "start": 0.55, "end": 0.90 }


# ============================================================
# GENE SCHEMA
# ============================================================

const GENE_SCHEMA := {
	# HSV for gradient stop A
	"hue_a": { "type": "hue",   "min": 0.0, "max": 1.0, "mut_amount": 0.10 },
	"sat_a": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },
	"val_a": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },

	# HSV for gradient stop B
	"hue_b": { "type": "hue",   "min": 0.0, "max": 1.0, "mut_amount": 0.10 },
	"sat_b": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },
	"val_b": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },

	"mut_rate": { "type": "float", "min": 0.0, "max": 0.35, "mut_amount": 0.08 },
	"lifespan": { "type": "float", "min": 6.0, "max": 120.0, "mut_amount": 6.0 },

	"speed": { "type": "float", "min": 40.0, "max": 360.0, "mut_amount": 25.0 },

	"social": { "type": "float", "min": 0.0, "max": 2.0, "mut_amount": 0.25 },
	"social_strength": { "type": "float", "min": 0.0, "max": 0.5, "mut_amount": 0.05 },

	"wander_rate": { "type": "float", "min": 0.05, "max": 2.5, "mut_amount": 0.15 },
	"wander_keep": { "type": "float", "min": 0.0, "max": 1.0, "mut_amount": 0.10 },

	"mate_cooldown": { "type": "float", "min": 0.2, "max": 3.0, "mut_amount": 0.5 },
	"fertile_start": { "type": "float", "min": 0.0, "max": 0.2, "mut_amount": 0.08 },
	"fertile_end":   { "type": "float", "min": 0.6, "max": 1.0, "mut_amount": 0.08 },
	"species_threshold": { "type": "float", "min": 0.4, "max": 0.9, "mut_amount": 0.08 },

	"glow_max":        { "type": "float", "min": 0.0, "max": 2.0, "mut_amount": 0.25 },
	"glow_pulse_rate": { "type": "float", "min": 0.001, "max": 0.02, "mut_amount": 0.003 },
	"glow_ramp":       { "type": "float", "min": 0.5, "max": 15.0, "mut_amount": 2.0 },

	"petal_count":  { "type": "int",   "min": 3,   "max": 12,  "mut_amount": 2 },
	"petal_length": { "type": "float", "min": 0.5, "max": 3.0, "mut_amount": 0.15 },
	"petal_width":  { "type": "float", "min": 0.2, "max": 2.0, "mut_amount": 0.15 },
	"flower_size":  { "type": "float", "min": 0.5, "max": 3.0, "mut_amount": 0.20 },

	"leaf_shape": { "type": "enum", "values": ["round", "oval", "lance", "heart"], "mut_amount": 1 },
	"leaf_edge":  { "type": "enum", "values": ["smooth", "serrated", "lobed"],     "mut_amount": 1 },

	"spot_pattern":  { "type": "enum",  "values": ["none", "freckle", "polka", "ring"], "mut_amount": 1 },
	"vein_contrast": { "type": "float", "min": 0.0, "max": 1.0, "mut_amount": 0.18 },

	"toxic": { "type": "bool", "mut_amount": 1 },
	"scent": { "type": "enum", "values": ["none", "sweet", "funk", "sharp"], "mut_amount": 1 },

	"inv_glow_lock": { "type": "bool", "mut_amount": 1 }
}

# ============================================================
# DIPLOID GENOME + PHENOTYPE
# ============================================================

# Two haplotypes per chromosome (diploid)
var chr1_a: Dictionary = {}
var chr1_b: Dictionary = {}
var chr2_a: Dictionary = {}
var chr2_b: Dictionary = {}

# Expressed phenotype (what behavior + drawing uses)
var genes: Dictionary = {}


# ============================================================
# STATE
# ============================================================
var age := 0.0
var lifespan := 20.0
var dead := false

var velocity := Vector2.ZERO
var wander_dir := Vector2.RIGHT

var can_mate := true
var world_rect := Rect2()


# ============================================================
# READY
# ============================================================

func _ready() -> void:
	add_to_group("cells")
	world_rect = get_viewport_rect()

	# We draw procedurally in _draw() so sprite art is optional.
	if sprite:
		sprite.visible = false

	queue_redraw()

	# Make gradient unique per instance (safe if sprite has GradientTexture2D)
	if sprite and sprite.texture:
		var tex := sprite.texture as GradientTexture2D
		if tex != null:
			tex = tex.duplicate(true)
			if tex.gradient != null:
				tex.gradient = tex.gradient.duplicate(true)
			sprite.texture = tex

	# Founder roll if diploid genome is empty
	if chr1_a.is_empty() or chr1_b.is_empty() or chr2_a.is_empty() or chr2_b.is_empty():
		roll_random_diploid_genome()

	rebuild_expressed_genes()

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
# LABEL / UI
# ============================================================

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
	lines.append("inv_glow_lock (expr): %s" % str(bool(genes.get("inv_glow_lock", false))))
	return "\n".join(lines)

func gene_card_short() -> String:
	return "sp=%s petals=%d (L%.2f W%.2f) flower=%.2f leaf=%s/%s spots=%s veins=%.2f glow=%.2f toxic=%s scent=%s inv_expr=%s" % [
		species_tag(),
		int(genes.get("petal_count", -1)),
		float(genes.get("petal_length", 0.0)),
		float(genes.get("petal_width", 0.0)),
		float(genes.get("flower_size", 1.0)),
		str(genes.get("leaf_shape", "?")),
		str(genes.get("leaf_edge", "?")),
		str(genes.get("spot_pattern", "?")),
		float(genes.get("vein_contrast", 0.0)),
		float(genes.get("glow_max", 0.0)),
		str(bool(genes.get("toxic", false))),
		str(genes.get("scent", "none")),
		str(bool(genes.get("inv_glow_lock", false)))
	]


# ============================================================
# DIPLOID: FOUNDERS + EXPRESSION
# ============================================================

func roll_random_haplotype() -> Dictionary:
	var h := {}
	for key in GENE_SCHEMA:
		var s = GENE_SCHEMA[key]
		match s["type"]:
			"float":
				h[key] = randf_range(float(s["min"]), float(s["max"]))
			"hue":
				h[key] = randf()
			"int":
				h[key] = randi_range(int(s["min"]), int(s["max"]))
			"bool":
				h[key] = randf() < 0.5
			"enum":
				var vals: Array = s["values"]
				h[key] = vals[randi() % vals.size()]
			_:
				pass

	if not h.has("inv_glow_lock"):
		h["inv_glow_lock"] = randf() < 0.25

	return h

func roll_random_diploid_genome() -> void:
	# We store full gene sets on each haplotype.
	# Recombination only *uses* the loci lists, but it’s fine if the dict contains extra keys.
	chr1_a = roll_random_haplotype()
	chr1_b = roll_random_haplotype()
	chr2_a = roll_random_haplotype()
	chr2_b = roll_random_haplotype()

func _circular_mean_hue(a: float, b: float) -> float:
	var ang_a = a * TAU
	var ang_b = b * TAU
	var x = cos(ang_a) + cos(ang_b)
	var y = sin(ang_a) + sin(ang_b)
	if abs(x) < 0.00001 and abs(y) < 0.00001:
		return a
	var ang = atan2(y, x)
	return fposmod(ang / TAU, 1.0)

func _enum_dominant_value(gene_key: String, a_val, b_val):
	# Dominance-by-order model:
	# lowest index in schema values wins
	if not GENE_SCHEMA.has(gene_key):
		return a_val
	var s = GENE_SCHEMA[gene_key]
	if not s.has("values"):
		return a_val
	var vals: Array = s["values"]
	var ai := vals.find(a_val)
	var bi := vals.find(b_val)
	if ai == -1 and bi == -1:
		return a_val
	if ai == -1:
		return b_val
	if bi == -1:
		return a_val
	return a_val if ai <= bi else b_val

func express_alleles(key: String, a, b):
	if not GENE_SCHEMA.has(key):
		return a

	var s = GENE_SCHEMA[key]
	match s["type"]:
		"float":
			return (float(a) + float(b)) * 0.5
		"int":
			return int(round((float(a) + float(b)) * 0.5))
		"hue":
			return _circular_mean_hue(float(a), float(b))
		"bool":
			# Dominant-true model
			return bool(a) or bool(b)
		"enum":
			return _enum_dominant_value(key, a, b)
		_:
			return a

func rebuild_expressed_genes() -> void:
	var out := {}
	for key in GENE_SCHEMA:
		var a_val = null
		var b_val = null

		# This project keeps CHR1 and CHR2 as the only sources of truth.
		# If a key exists on both, we prefer chr2 for “visual-ish” things only by accident.
		# We resolve by membership in the loci lists.
		if _gene_on_chr1(key):
			a_val = chr1_a.get(key)
			b_val = chr1_b.get(key)
		else:
			a_val = chr2_a.get(key)
			b_val = chr2_b.get(key)

		if a_val == null or b_val == null:
			continue

		out[key] = express_alleles(String(key), a_val, b_val)

	genes = out
	normalize_fertility_window(genes)

func _gene_on_chr1(key: String) -> bool:
	for l in CHR1_LOCI:
		if String(l["name"]) == key:
			return true
	return false


# ============================================================
# VALIDATION HELPERS
# ============================================================

func normalize_fertility_window(g: Dictionary) -> void:
	var fs = float(g.get("fertile_start", 0.1))
	var fe = float(g.get("fertile_end", 0.8))

	fs = clamp(fs, 0.0, 0.95)
	fe = clamp(fe, 0.05, 1.0)

	if fs >= fe:
		fs = max(0.0, fe - 0.1)

	g["fertile_start"] = fs
	g["fertile_end"] = fe


# ============================================================
# MUTATION HELPERS
# ============================================================

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


# ============================================================
# RECOMBINATION UTILITIES (linked loci + lam + hotspots + inversions)
# ============================================================

func poisson(lam: float) -> int:
	var L = exp(-lam)
	var k = 0
	var p = 1.0
	while p > L:
		k += 1
		p *= randf()
	return k - 1

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

func sample_cut_respecting(cfg: Dictionary, suppressed_spans: Array, max_tries := 24) -> float:
	for i in range(max_tries):
		var c = sample_cut(cfg)
		if not cut_is_suppressed(c, suppressed_spans):
			return c
	return -1.0

func get_suppressed_spans_for_chr2_parent(hap_a: Dictionary, hap_b: Dictionary) -> Array:
	var spans := []
	var inv_a = bool(hap_a.get("inv_glow_lock", false))
	var inv_b = bool(hap_b.get("inv_glow_lock", false))
	if inv_a != inv_b:
		spans.append([float(INV_GLOW_LOCK["start"]), float(INV_GLOW_LOCK["end"])])
	return spans

func recombine_linked(hap_a: Dictionary, hap_b: Dictionary, loci: Array, cfg: Dictionary, suppressed_spans: Array) -> Dictionary:
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

	# Choose initial source randomly to avoid bias
	if randf() < 0.5:
		flip = true

	for locus in loci:
		var pos := float(locus["pos"])
		var name := String(locus["name"])

		while ci < cuts.size() and pos > float(cuts[ci]):
			flip = !flip
			ci += 1

		out[name] = hap_b.get(name) if flip else hap_a.get(name)

	return out


# ============================================================
# MEIOSIS + DIPLOID CHILD CREATION
# ============================================================

func mutate_gamete(gam: Dictionary, mut_rate: float) -> void:
	for key in gam.keys():
		gam[key] = mutate_value_by_schema(String(key), gam[key], mut_rate)

func make_gamete() -> Dictionary:
	# Mut rate for this meiosis: use expressed mut_rate
	var mr = float(genes.get("mut_rate", 0.10))

	# chr1: recombine between this individual's two chr1 haplotypes
	var gam1 = recombine_linked(chr1_a, chr1_b, CHR1_LOCI, CHR_CFG[1], [])
	mutate_gamete(gam1, mr)

	# chr2: recombine between this individual's two chr2 haplotypes (possibly suppressed)
	var suppressed2 = get_suppressed_spans_for_chr2_parent(chr2_a, chr2_b)
	var gam2 = recombine_linked(chr2_a, chr2_b, CHR2_LOCI, CHR_CFG[2], suppressed2)
	mutate_gamete(gam2, mr)

	return { "chr1": gam1, "chr2": gam2 }

func build_child_from_parents(parent_a: Node, parent_b: Node) -> void:
	var ga = parent_a.call("make_gamete")
	var gb = parent_b.call("make_gamete")

	chr1_a = ga["chr1"]
	chr1_b = gb["chr1"]
	chr2_a = ga["chr2"]
	chr2_b = gb["chr2"]

	rebuild_expressed_genes()


# ============================================================
# APPLY GENES (visual + derived state)
# ============================================================

func apply_genes() -> void:
	# Apply gradient if sprite has GradientTexture2D
	if sprite and sprite.texture:
		var tex := sprite.texture as GradientTexture2D
		if tex != null and tex.gradient != null:
			var ca = Color.from_hsv(float(genes.get("hue_a", 0.0)), float(genes.get("sat_a", 1.0)), float(genes.get("val_a", 1.0)), 1.0)
			var cb = Color.from_hsv(float(genes.get("hue_b", 0.0)), float(genes.get("sat_b", 1.0)), float(genes.get("val_b", 1.0)), 1.0)
			tex.gradient.set_color(0, ca)
			tex.gradient.set_color(1, cb)

	# Optional scale hook (even if sprite hidden, it won’t hurt)
	var fs = float(genes.get("flower_size", 1.0))
	if sprite:
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

	# Update collision capsule to match procedural flower size
	update_collision_capsule()

	queue_redraw()


# ============================================================
# COLLISION SHAPE UPDATE (CAPSULE)
# ============================================================

func update_collision_capsule() -> void:
	# Requires:
	#   CollisionShape2D named "CollisionShape2D"
	#   with CapsuleShape2D assigned.
	if collision_shape == null:
		return

	var cap := collision_shape.shape as CapsuleShape2D
	if cap == null:
		return

	var flower_size: float = float(genes.get("flower_size", 1.0))
	var petal_length: float = float(genes.get("petal_length", 1.0))

	# This is the same base radius used in _draw()
	var base_radius: float = 20.0 * flower_size

	# Capsule radius: "body width" of the plant (includes petals a bit)
	var r: float = base_radius * (0.55 + 0.10 * clamp(petal_length, 0.5, 3.0))

	# Capsule reach: petals extend outward, so increase half-length with petal_length
	var half_len: float = base_radius * (0.50 + 0.55 * clamp(petal_length, 0.5, 3.0))

	cap.radius = max(6.0, r)
	# CapsuleShape2D.height is the middle section height (excluding the semicircle caps).
	cap.height = max(0.0, (half_len * 2.0) - (cap.radius * 2.0))


# ============================================================
# DRAWING UTILITIES
# ============================================================

func quad_bezier(p0: Vector2, p1: Vector2, p2: Vector2, t: float) -> Vector2:
	var a = p0.lerp(p1, t)
	var b = p1.lerp(p2, t)
	return a.lerp(b, t)

func build_petal_outline(base: Vector2, tip: Vector2, left_ctrl: Vector2, right_ctrl: Vector2, steps: int) -> PackedVector2Array:
	var pts := PackedVector2Array()

	for i in range(steps + 1):
		var t = float(i) / float(steps)
		pts.append(quad_bezier(base, left_ctrl, tip, t))

	for i in range(steps, -1, -1):
		var t = float(i) / float(steps)
		pts.append(quad_bezier(base, right_ctrl, tip, t))

	return pts

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
		var t = float(i) / float(max(1, points.size() - 1))
		var side_weight = sin(PI * t)

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

func build_heart_outline(base: Vector2, tip: Vector2, width: float, steps: int) -> PackedVector2Array:
	var axis = (tip - base)
	var axis_len = axis.length()
	if axis_len <= 0.001:
		return PackedVector2Array()

	var forward = axis / axis_len
	var right = forward.rotated(PI / 2.0)

	# Shape tuning
	var notch_depth: float = axis_len * 0.18
	var lobe_height: float = axis_len * 0.35
	var tip_taper: float   = 0.55

	width = max(width, axis_len * 0.05)

	var pts := PackedVector2Array()

	var notch = base + forward * (notch_depth * 0.55)
	var notch_left  = base + forward * (notch_depth * 0.25) + right * (width * 0.20)
	var notch_right = base + forward * (notch_depth * 0.25) - right * (width * 0.20)

	var lobe_left  = base + forward * lobe_height + right * (width * 0.55)
	var lobe_right = base + forward * lobe_height - right * (width * 0.55)

	var tip_point = tip

	# notch -> lobe_left
	var seg: int = maxi(4, steps / 6)

	for i in range(seg + 1):
		var t = float(i) / float(seg)
		pts.append(quad_bezier(notch, notch_left, lobe_left, t))

	# lobe_left -> tip with taper
	var seg2: int = maxi(6, steps / 3)

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

	# tip -> lobe_right (reverse)
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

func _safe_draw_polygon(points: PackedVector2Array, color: Color) -> void:
	# Prevent "Invalid polygon data, triangulation failed."
	if points.size() < 3:
		return
	var idx := Geometry2D.triangulate_polygon(points)
	if idx.is_empty():
		return
	draw_colored_polygon(points, color)


# ============================================================
# DRAW
# ============================================================

func _draw() -> void:
	if genes.is_empty():
		return

	# Stable RNG so freckles don't flicker across frames
	var rng := RandomNumberGenerator.new()
	rng.seed = int(get_instance_id())

	var petal_count: int = int(genes.get("petal_count", 6))
	var petal_length: float = float(genes.get("petal_length", 0.8))
	var petal_width: float = float(genes.get("petal_width", 0.6))
	var flower_size: float = float(genes.get("flower_size", 1.0))

	var leaf_shape: String = str(genes.get("leaf_shape", "oval"))
	var leaf_edge: String  = str(genes.get("leaf_edge", "smooth"))

	var spot_pattern: String = str(genes.get("spot_pattern", "none"))
	var vein_contrast: float = float(genes.get("vein_contrast", 0.0))

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

	var radius: float = 20.0 * flower_size
	var center := Vector2.ZERO

	# --- Glow halo ---
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
	var steps := 12

	for i in range(max(1, petal_count)):
		var angle = TAU * float(i) / float(max(1, petal_count))
		var dir = Vector2.RIGHT.rotated(angle)
		var side = dir.rotated(PI / 2.0)

		var base_in: float = 0.20
		var length_mult: float = petal_length
		var width_mult: float = petal_width

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
			_:
				# unknown enum value, treat as oval-ish
				length_mult *= 0.95
				width_mult *= 1.05
				base_in = 0.22

		var base = center + dir * (radius * base_in)
		var tip = center + dir * (radius * length_mult)

		var w = radius * width_mult * 0.55
		w = clamp(w, radius * 0.10, radius * 0.95)

		var forward = radius * length_mult * 0.30
		var base_push = radius * 0.10
		var left_ctrl = base + dir * (forward + base_push) + side * w
		var right_ctrl = base + dir * (forward + base_push) - side * w

		var pts: PackedVector2Array
		if leaf_shape == "heart":
			var heart_w = clamp(w * 1.15, radius * 0.12, radius * 0.95)
			pts = build_heart_outline(base, tip, heart_w, 64)
		else:
			pts = build_petal_outline(base, tip, left_ctrl, right_ctrl, steps)

		var edge_intensity = clamp(vein_contrast, 0.0, 1.0)
		pts = apply_edge_style(pts, base, tip, leaf_edge, edge_intensity)

		_safe_draw_polygon(pts, base_color)

		# Veins
		if vein_contrast > 0.05:
			var vein_color = base_color.lerp(Color.WHITE, vein_contrast)
			draw_line(base, tip, vein_color, 1.2 + 1.2 * vein_contrast)

			if leaf_shape == "heart" and vein_contrast > 0.15:
				var vein_color2 = base_color.lerp(Color.WHITE, vein_contrast * 0.8)
				draw_line(base, base + (tip - base) * 0.75 + side * (w * 0.25), vein_color2, 1.0)
				draw_line(base, base + (tip - base) * 0.75 - side * (w * 0.25), vein_color2, 1.0)

	# Center disk
	draw_circle(center, radius * 0.35, center_color)

	# Spots
	match spot_pattern:
		"freckle":
			for j in range(8):
				var rr = rng.randf_range(radius * 0.2, radius * 0.8)
				var aa = rng.randf() * TAU
				draw_circle(center + Vector2.RIGHT.rotated(aa) * rr, 2.0, center_color)
		"polka":
			for j in range(petal_count):
				var aa = TAU * float(j) / float(max(1, petal_count))
				var rr = radius * 0.65
				draw_circle(center + Vector2.RIGHT.rotated(aa) * rr, 3.0, center_color)
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
		if not (o is Node2D):
			continue
		var d2 = position.distance_squared_to((o as Node2D).position)
		if d2 < nearest_d2:
			nearest_d2 = d2
			nearest = o

	if nearest != null:
		var to_other = ((nearest as Node2D).position - position).normalized()
		var s = float(genes.get("social", 0.0)) * float(genes.get("social_strength", 0.0))
		wander_dir = (wander_dir * (1.0 - s) + to_other * s).normalized()


# ============================================================
# GLOW
# ============================================================

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

func _on_area_entered(other: Area2D) -> void:
	if other == self or not other.is_in_group("cells"):
		return

	# only one side initiates to avoid duplicates
	if get_instance_id() > other.get_instance_id():
		return

	if dead or not can_mate:
		return
	if "can_mate" in other and not other.can_mate:
		return

	# Fertility window check (phenotype)
	var fertile_start = float(genes.get("fertile_start", 0.1))
	var fertile_end = float(genes.get("fertile_end", 0.8))
	if age < lifespan * fertile_start:
		return
	if age > lifespan * fertile_end:
		return

	# Species gate
	var dist = species_distance(genes, other.genes)
	var my_thresh = float(genes.get("species_threshold", 0.5))
	if dist > my_thresh:
		return

	call_deferred("mate_with_deferred", other)

func mate_with_deferred(partner: Area2D) -> void:
	if dead:
		return
	if not is_instance_valid(partner):
		return
	if "dead" in partner and partner.dead:
		return

	mate_with(partner)

func mate_with(partner: Area2D) -> void:
	if not is_instance_valid(partner):
		return

	can_mate = false
	partner.can_mate = false

	var baby: Area2D = duplicate()
	baby.position = (position + partner.position) * 0.5 + Vector2(randf_range(-20.0, 20.0), randf_range(-20.0, 20.0))
	get_parent().add_child(baby)

	# DIPLOID: make baby from gametes
	baby.build_child_from_parents(self, partner)
	baby.apply_genes()
	baby.can_mate = false

	var cooldown: float = float(genes.get("mate_cooldown", 1.5))

	var self_ref: WeakRef = weakref(self)
	var partner_ref: WeakRef = weakref(partner)

	var t := get_tree().create_timer(cooldown)
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
# DIPLOID INSPECTOR + CHROMOSOME VISUALIZER
# ============================================================
##
## What you get:
## - Phenotype card (expressed genes)
## - Genotype card (A | B → expressed, with dominance rules)
## - Chromosome rails (CHR1/CHR2) with loci order + distances
## - Heterozygous highlighting + inversion shading for CHR2
##
## Requires:
## - chr1_a, chr1_b, chr2_a, chr2_b dictionaries exist
## - genes dictionary exists (phenotype/expression)
##
## Notes on dominance:
## - floats/hues: expressed = average (hue uses circular mean)
## - bool: expressed = OR (True dominates)
## - enum: we pick allele with higher dominance rank (see DOMINANCE)
## - int: expressed = round((a+b)/2) by default
##

# --- dominance ranks for enum genes ---
# Higher number wins when heterozygous.
const DOMINANCE := {
	"leaf_shape": {
		"heart": 4,
		"lance": 3,
		"oval": 2,
		"round": 1,
	},
	"leaf_edge": {
		"lobed": 3,
		"serrated": 2,
		"smooth": 1,
	},
	"spot_pattern": {
		"ring": 4,
		"polka": 3,
		"freckle": 2,
		"none": 1,
	},
	"scent": {
		"sharp": 4,
		"funk": 3,
		"sweet": 2,
		"none": 1,
	},
}

# --- helpful: circular mean for hue ---
func _hue_mean(a: float, b: float) -> float:
	var ang_a: float = a * TAU
	var ang_b: float = b * TAU
	var x: float = cos(ang_a) + cos(ang_b)
	var y: float = sin(ang_a) + sin(ang_b)
	if abs(x) < 0.000001 and abs(y) < 0.000001:
		return a # opposite hues: just keep a
	var ang: float = atan2(y, x)
	return fposmod(ang / TAU, 1.0)

func _enum_pick_dominant(key: String, a, b):
	if not DOMINANCE.has(key):
		return a # fallback: allele A dominates
	var ranks: Dictionary = DOMINANCE[key]
	var ra: int = int(ranks.get(a, 0))
	var rb: int = int(ranks.get(b, 0))
	return a if ra >= rb else b

func _format_val(v) -> String:
	# Keep it readable in UI
	if typeof(v) == TYPE_FLOAT:
		return "%.3f" % float(v)
	return str(v)

func _is_hetero(a, b) -> bool:
	return str(a) != str(b)

# ------------------------------------------------------------
# GENOTYPE + PHENOTYPE TEXT CARDS
# ------------------------------------------------------------

func phenotype_card_multiline() -> String:
	# This is basically your current inspector: expressed traits
	var lines: Array[String] = []
	lines.append("PHENOTYPE (expressed)")
	lines.append("petals: %s" % _format_val(genes.get("petal_count", "?")))
	lines.append("petal L/W: %s / %s" % [_format_val(genes.get("petal_length", "?")), _format_val(genes.get("petal_width", "?"))])
	lines.append("flower size: %s" % _format_val(genes.get("flower_size", "?")))
	lines.append("leaf: %s / %s" % [str(genes.get("leaf_shape", "?")), str(genes.get("leaf_edge", "?"))])
	lines.append("pattern: %s | veins %s" % [str(genes.get("spot_pattern", "?")), _format_val(genes.get("vein_contrast", "?"))])
	lines.append("glow_max: %s" % _format_val(genes.get("glow_max", "?")))
	lines.append("toxic: %s | scent: %s" % [str(bool(genes.get("toxic", false))), str(genes.get("scent", "none"))])
	lines.append("inv_glow_lock: %s" % str(bool(genes.get("inv_glow_lock", false))))
	return "\n".join(lines)

func genotype_card_multiline() -> String:
	# Shows allele A | allele B → expressed
	var lines: Array[String] = []
	lines.append("GENOTYPE (diploid alleles)")
	lines.append("Format: A | B  →  expressed")

	# pick a nice stable ordering: CHR1 loci then CHR2 loci then anything else
	var ordered_keys: Array[String] = []
	for l in CHR1_LOCI:
		ordered_keys.append(String(l["name"]))
	for l in CHR2_LOCI:
		ordered_keys.append(String(l["name"]))

	# also include schema keys not in loci (if any)
	for k in GENE_SCHEMA.keys():
		var ks: String = String(k)
		if not ordered_keys.has(ks):
			ordered_keys.append(ks)

	for key in ordered_keys:
		var a_val = _get_allele_for_key(key, true)
		var b_val = _get_allele_for_key(key, false)
		var expr = genes.get(key, "?")

		var het: bool = _is_hetero(a_val, b_val)
		var mark: String = " *" if het else ""
		lines.append("%s%s: %s | %s  →  %s" % [key, mark, _format_val(a_val), _format_val(b_val), _format_val(expr)])

	lines.append("\n* = heterozygous (A != B)")
	lines.append("\nDominance rules for enums:")
	lines.append("leaf_shape: heart > lance > oval > round")
	lines.append("leaf_edge: lobed > serrated > smooth")
	lines.append("spot_pattern: ring > polka > freckle > none")
	lines.append("scent: sharp > funk > sweet > none")
	lines.append("\n(You can tweak DOMINANCE in cell.gd.)")
	return "\n".join(lines)

func _get_allele_for_key(key: String, take_a: bool):
	# Routes key to the right chromosome dictionary.
	# If missing, falls back to expressed gene.
	var src: Dictionary

	var is_chr1: bool = false
	var is_chr2: bool = false

	for l in CHR1_LOCI:
		if String(l["name"]) == key:
			is_chr1 = true
			break
	for l2 in CHR2_LOCI:
		if String(l2["name"]) == key:
			is_chr2 = true
			break

	if is_chr1:
		src = chr1_a if take_a else chr1_b
	elif is_chr2:
		src = chr2_a if take_a else chr2_b
	else:
		return genes.get(key, "?")

	return src.get(key, genes.get(key, "?"))

# ------------------------------------------------------------
# CHROMOSOME VISUALIZER TEXT (rails + distances)
# ------------------------------------------------------------

func chromosomes_visualizer_text() -> String:
	var lines: Array[String] = []
	lines.append("CHROMOSOMES (loci order + distances)")
	lines.append("Legend: [A] top rail, [B] bottom rail, * heterozygous")
	lines.append("Distances shown are Δpos in 0..1 space (not real cM, but useful linkage intuition).")
	lines.append("")

	lines.append(_chromosome_block("CHR1 (behavior)", CHR1_LOCI, chr1_a, chr1_b, null))
	lines.append("")
	lines.append(_chromosome_block("CHR2 (visual/chemistry)", CHR2_LOCI, chr2_a, chr2_b, INV_GLOW_LOCK))
	return "\n".join(lines)

func _chromosome_block(title: String, loci: Array, a: Dictionary, b: Dictionary, inv_span) -> String:
	var out: Array[String] = []
	out.append(title)

	# Optional inversion shading line (CHR2)
	if inv_span != null:
		var s: float = float(inv_span["start"])
		var e: float = float(inv_span["end"])
		out.append("INV span: [%.2f .. %.2f] (suppresses crossovers when parents differ)" % [s, e])

	# header
	out.append("pos    Δpos   gene                  A allele        B allele        expr")
	out.append("-----  -----  --------------------  ------------    ------------    ------------")

	var prev_pos: float = 0.0
	for i in range(loci.size()):
		var loc: Dictionary = loci[i]
		var name: String = String(loc["name"])
		var pos: float = float(loc["pos"])
		var dpos: float = pos - prev_pos
		prev_pos = pos

		var av = a.get(name, "?")
		var bv = b.get(name, "?")
		var expr = genes.get(name, "?")
		var het: bool = _is_hetero(av, bv)
		var mark: String = "*" if het else " "

		# Inversion marker for loci inside span
		var inv_mark: String = " "
		if inv_span != null:
			var s2: float = float(inv_span["start"])
			var e2: float = float(inv_span["end"])
			if pos >= s2 and pos <= e2:
				inv_mark = "I"

		out.append("%0.2f  %0.2f   %s%s   %-16s  %-12s      %-12s      %-12s" % [
			pos,
			dpos,
			inv_mark,
			mark,
			name,
			_format_val(av),
			_format_val(bv),
			_format_val(expr)
		])

	out.append("Marks: * heterozygous, I inside inversion span")
	return "\n".join(out)


# ============================================================
# ID HELPERS
# ============================================================

func short_id() -> String:
	return str(get_instance_id() % 100000)

func species_tag() -> String:
	return str(species_id % 1000)

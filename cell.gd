extends Area2D

@onready var sprite: Sprite2D = $Sprite2D 
@onready var wander_timer: Timer = $WanderTimer
@onready var species_label: Label = $SpeciesLabel


var species_id := 0
var glow_strength := 0.0
var selected := false
var _base_label_text := ""
var show_genetics := false

var world: Node = null

func set_world(w: Node) -> void:
	world = w

signal cell_selected(cell)


# ============================================================
# CHROMOSOMES (2 total): linked loci, per-chromosome lam, hotspots, inversions
# ============================================================

# Loci positions are 0..1 along each chromosome. Nearby loci tend to inherit together.
const CHR1_LOCI := [
	# "Life / behavior / timing" bundle
	{ "name": "mut_rate", "pos": 0.05 },
	{ "name": "lifespan", "pos": 0.12 },

	{ "name": "speed", "pos": 0.22 },

	{ "name": "social", "pos": 0.34 },
	{ "name": "social_strength", "pos": 0.40 },

	{ "name": "wander_rate", "pos": 0.55 },
	{ "name": "wander_keep", "pos": 0.62 },

	{ "name": "mate_cooldown", "pos": 0.25 },
	{ "name": "fertile_start", "pos": 0.82 },
	{ "name": "fertile_end", "pos": 0.88 },
	{ "name": "species_threshold", "pos": 0.95 },
]

const CHR2_LOCI := [
	# "Plant weirdness / visuals / chemistry" bundle
	{ "name": "petal_count", "pos": 0.08 },
	{ "name": "petal_length", "pos": 0.16 },
	{ "name": "petal_width", "pos": 0.22 },
	{ "name": "flower_size", "pos": 0.30 },

	{ "name": "leaf_shape", "pos": 0.46 },
	{ "name": "leaf_edge", "pos": 0.52 },

	# Pattern / glow / toxic live in a region that can be "locked" by an inversion
	{ "name": "spot_pattern", "pos": 0.60 },
	{ "name": "vein_contrast", "pos": 0.66 },

	{ "name": "glow_max", "pos": 0.72 },
	{ "name": "glow_pulse_rate", "pos": 0.76 },
	{ "name": "glow_ramp", "pos": 0.79 },

	{ "name": "toxic", "pos": 0.86 },
	{ "name": "scent", "pos": 0.92 },

	# Your existing gradient genes also live here (linked color chunks!)
	{ "name": "hue_a", "pos": 0.10 },
	{ "name": "sat_a", "pos": 0.12 },
	{ "name": "val_a", "pos": 0.14 },
	{ "name": "hue_b", "pos": 0.18 },
	{ "name": "sat_b", "pos": 0.20 },
	{ "name": "val_b", "pos": 0.24 },
]

# Per-chromosome recombination tuning
# lam = expected crossovers per meiosis per chromosome

# λ value	What happens
# 0.0	No recombination. Whole chromosome inherited from one parent. Pure lineages.
# 0.5	Usually 0–1 crossovers. Big trait chunks stay together.
# 1.0	About 1 crossover per chromosome. Moderate mixing.
# 2.0	2-ish crossovers. Smaller chunks. More scrambling.
# 5.0	Genetic confetti.

const CHR_CFG := {
	1: {
		"lam": 2.5,
		"hotspots": [
			{ "start": 0.50, "end": 0.70, "weight": 3.0 }, # breaks wander/mating/timing bundles sometimes
		],
		"coldspots": []
	},
	2: {
		"lam": 2.5,
		"hotspots": [
			{ "start": 0.65, "end": 0.82, "weight": 4.0 }, # the "weirdness" region recombines a bit more often
		],
		"coldspots": []
	}
}

# Inversion that suppresses recombination in a segment (when heterozygous between parents)
# This one "locks" pattern+glow+toxic so they travel together as a lineage chunk.
const INV_GLOW_LOCK := { "chr": 2, "start": 0.55, "end": 0.90 }

# ============================================================
# GENE SCHEMA
# ============================================================

const GENE_SCHEMA := {
	# HSV for gradient stop A
	"hue_a": { "type": "hue", "min": 0.0, "max": 1.0, "mut_amount": 0.10 },
	"sat_a": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },
	"val_a": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },

	# HSV for gradient stop B
	"hue_b": { "type": "hue", "min": 0.0, "max": 1.0, "mut_amount": 0.10 },
	"sat_b": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },
	"val_b": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },

	"mut_rate": { "type": "float", "min": 0.0, "max": 0.35, "mut_amount": 0.08 },
	"lifespan": { "type": "float", "min": 6.0, "max": 120.0, "mut_amount": 6.0 },

	"speed": { "type": "float", "min": 40.0, "max": 360.0, "mut_amount": 25.0 },

	"social": { "type": "float", "min": 0.0, "max": 2.0, "mut_amount": 0.25 },
	"social_strength": { "type": "float", "min": 0.0, "max": 0.5, "mut_amount": 0.05 },

	"wander_rate": { "type": "float", "min": 0.05, "max": 2.5, "mut_amount": 0.15 },
	"wander_keep": { "type": "float", "min": 0.0, "max": 1.0, "mut_amount": 0.10 },

	"mate_cooldown": { "type": "float", "min": 0.2, "max": 1.0, "mut_amount": 0.5 },
	"fertile_start": { "type": "float", "min": 0.0, "max": 0.2, "mut_amount": 0.08 },
	"fertile_end": { "type": "float", "min": 0.6, "max": 1.0, "mut_amount": 0.08 },
	"species_threshold": { "type": "float", "min": 0.4, "max": 0.9, "mut_amount": 0.08 },

	"glow_max": { "type": "float", "min": 1.0, "max": 2, "mut_amount": 0.25 },
	"glow_pulse_rate": { "type": "float", "min": 0.001, "max": 0.02, "mut_amount": 0.003 },
	"glow_ramp": { "type": "float", "min": 0.5, "max": 15.0, "mut_amount": 2.0 },

	# ============================================================
	# NEW: Plant weirdness genes (mixed discrete + sliders)
	# ============================================================

	# Petals CAN mutate beyond a fixed set.
	"petal_count": { "type": "int", "min": 3, "max": 12, "mut_amount": 2 }, # mut_amount = typical step size
	"petal_length": { "type": "float", "min": .5, "max": 3.0, "mut_amount": 0.15 },
	"petal_width": { "type": "float", "min": 0.2, "max": 2.0, "mut_amount": 0.15 },
	"flower_size": { "type": "float", "min": 0.5, "max": 3.0, "mut_amount": 0.20 },

	"leaf_shape": { "type": "enum", "values": ["round", "oval", "lance", "heart"], "mut_amount": 1 },
	"leaf_edge": { "type": "enum", "values": ["smooth", "serrated", "lobed"], "mut_amount": 1 },

	"spot_pattern": { "type": "enum", "values": ["none", "freckle", "polka", "ring"], "mut_amount": 1 },
	"vein_contrast": { "type": "float", "min": 0.0, "max": 1.0, "mut_amount": 0.18 },

	"toxic": { "type": "bool", "mut_amount": 1 },
	"scent": { "type": "enum", "values": ["none", "sweet", "funk", "sharp"], "mut_amount": 1 },

	# Chromosome 2 inversion toggle. If parents differ, recombination suppressed in INV_GLOW_LOCK span.
	"inv_glow_lock": { "type": "bool", "mut_amount": 1 }
}

var genes := {}

func set_genetics_visible(on: bool) -> void:
	show_genetics = on
	# If you're using your refresh_label() + selected logic:
	if has_method("refresh_label"):
		refresh_label()
	else:
		# fallback: keep label as species id if no refresh_label exists
		if species_label != null:
			species_label.visible = true


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

func _ready():
	add_to_group("cells")
	world_rect = get_viewport_rect()
	
	if sprite:
		sprite.visible = false
	queue_redraw()

	# Make gradient unique per instance
	var tex := sprite.texture as GradientTexture2D
	if tex != null:
		tex = tex.duplicate(true)
		if tex.gradient != null:
			tex.gradient = tex.gradient.duplicate(true)
		sprite.texture = tex

	# Founder roll if empty
	if genes.is_empty():
		genes = roll_random_genes()

	if genes.is_empty():
		genes = roll_random_genes()
		normalize_fertility_window(genes)

	lifespan = genes["lifespan"]

	apply_genes()

	wander_timer.wait_time = genes["wander_rate"]

	if not wander_timer.timeout.is_connected(_on_wander_timer_timeout):
		wander_timer.timeout.connect(_on_wander_timer_timeout)

	if not area_entered.is_connected(_on_area_entered):
		area_entered.connect(_on_area_entered)

	_on_wander_timer_timeout()

# ============================================================
# GENE SYSTEM
# ============================================================

func roll_random_genes() -> Dictionary:
	var g := {}
	for key in GENE_SCHEMA:
		var s = GENE_SCHEMA[key]
		match s["type"]:
			"float":
				g[key] = randf_range(s["min"], s["max"])
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

func choose_parent_float(a: float, b: float) -> float:
	return a if randf() < 0.5 else b

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

func poisson(lam: float) -> int:
	var L = exp(-lam)
	var k = 0
	var p = 1.0
	while p > L:
		k += 1
		p *= randf()
	return k - 1

func apply_modifiers(intervals: Array, mods: Array) -> Array:
	# intervals: [ [a,b,w], ... ], mods: [ {start,end,weight}, ... ]
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

func get_suppressed_spans_for_chr2(a: Dictionary, b: Dictionary) -> Array:
	# If inversion differs between parents, suppress crossovers in the inversion span.
	var spans := []
	var inv_a = bool(a.get("inv_glow_lock", false))
	var inv_b = bool(b.get("inv_glow_lock", false))
	if inv_a != inv_b:
		spans.append([float(INV_GLOW_LOCK["start"]), float(INV_GLOW_LOCK["end"])])
	return spans

func recombine_linked(a: Dictionary, b: Dictionary, loci: Array, cfg: Dictionary, suppressed_spans: Array) -> Dictionary:
	# Produces ONE haploid-ish recombinant "chromatid" dict for these loci, by:
	# 1) generating cutpoints (Poisson with lam)
	# 2) walking loci in order and alternating parent source across cuts
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

	# Choose initial source randomly so recombination isn't biased.
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

func recombine_genes(a: Dictionary, b: Dictionary) -> Dictionary:
	# Linked recombination across 2 chromosomes + per-chr lam + hotspots + inversion suppression.
	var child := {}
	var cmut = lerp(float(a["mut_rate"]), float(b["mut_rate"]), randf())

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

	# Any genes not covered by chromosomes still inherit in your old "independent" style.
	for key in GENE_SCHEMA:
		if child.has(key):
			continue
		var s = GENE_SCHEMA[key]
		match s["type"]:
			"float", "hue", "int", "bool", "enum":
				var v = choose_parent_any(a.get(key), b.get(key))
				child[key] = v
			_:
				pass

	# Mutate everything according to schema (this is where your sliders and discrete genes shift)
	for key in child.keys():
		child[key] = mutate_value_by_schema(key, child[key], cmut)

	# Ensure fertility window stays sane after mutation/recombination
	normalize_fertility_window(child)

	return child

# ============================================================
# APPLY GENES
# ============================================================

func apply_genes():
	var tex := sprite.texture as GradientTexture2D
	if tex != null and tex.gradient != null:
		var ca = Color.from_hsv(float(genes["hue_a"]), float(genes["sat_a"]), float(genes["val_a"]), 1.0)
		var cb = Color.from_hsv(float(genes["hue_b"]), float(genes["sat_b"]), float(genes["val_b"]), 1.0)
		tex.gradient.set_color(0, ca)
		tex.gradient.set_color(1, cb)

	# A simple, safe visual hook for "flower_size" without needing new art:
	# (keeps your existing gradient and glow behavior)
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


		# Tiny hinting: toxic plants tint the label slightly (no new nodes needed)
		var tox = bool(genes.get("toxic", false))
		species_label.modulate = Color(1, 0.9, 0.9, 1) if tox else Color(1, 1, 1, 1)
		
		queue_redraw()
		
 

func build_petal_points(base: Vector2, tip: Vector2, left_ctrl: Vector2, right_ctrl: Vector2, steps: int) -> PackedVector2Array:
	var pts := PackedVector2Array()

	# Left edge: base -> tip
	for i in range(steps + 1):
		var t = float(i) / float(steps)
		pts.append(quad_bezier(base, left_ctrl, tip, t))

	# Right edge: tip -> base (reverse direction)
	for i in range(steps, -1, -1):
		var t = float(i) / float(steps)
		pts.append(quad_bezier(base, right_ctrl, tip, t))

	return pts

func quad_bezier(p0: Vector2, p1: Vector2, p2: Vector2, t: float) -> Vector2:
	var a = p0.lerp(p1, t)
	var b = p1.lerp(p2, t)
	return a.lerp(b, t)

func build_petal_outline(base: Vector2, tip: Vector2, left_ctrl: Vector2, right_ctrl: Vector2, steps: int) -> PackedVector2Array:
	var pts := PackedVector2Array()

	# Left edge: base -> tip
	for i in range(steps + 1):
		var t = float(i) / float(steps)
		pts.append(quad_bezier(base, left_ctrl, tip, t))

	# Right edge: tip -> base
	for i in range(steps, -1, -1):
		var t = float(i) / float(steps)
		pts.append(quad_bezier(base, right_ctrl, tip, t))

	return pts

func apply_edge_style(points: PackedVector2Array, base: Vector2, tip: Vector2, edge: String, intensity: float) -> PackedVector2Array:
	# edge styles: smooth/serrated/lobed
	# intensity 0..1 (use vein_contrast or a derived amount)
	if edge == "smooth" or points.size() < 6 or intensity <= 0.01:
		return points

	var axis = (tip - base)
	var axis_len = axis.length()
	if axis_len <= 0.001:
		return points
	axis /= axis_len
	var normal = axis.rotated(PI / 2.0)

	var out := PackedVector2Array()
	out.resize(points.size())

	# do not disturb the first/last few points too much (near base and tip)
	for i in range(points.size()):
		var p = points[i]
		var t = float(i) / float(max(1, points.size() - 1)) # 0..1 around outline
		# We want edge effects mostly on the sides, not base/tip
		var side_weight = sin(PI * t) # 0 at ends, 1 mid

		var jitter = 0.0
		match edge:
			"serrated":
				# high frequency small teeth
				jitter = sin(t * TAU * 10.0) * 0.6
			"lobed":
				# low frequency big lobes
				jitter = sin(t * TAU * 3.0) * 1.2
			_:
				jitter = 0.0

		var amp = intensity * side_weight * 3.0
		out[i] = p + normal * (jitter * amp)

	return out


func _draw():
	if genes.is_empty():
		return

	
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
	var fertile_start = float(genes.get("fertile_start", 0.1))
	var fertile_end = float(genes.get("fertile_end", 0.8))
	var fertile = age >= lifespan * fertile_start and age <= lifespan * fertile_end

	var glow_target = float(genes.get("glow_max", 0.0)) if can_mate and fertile and not dead else 0.0
	var pulse = 0.5 + 0.5 * sin(Time.get_ticks_msec() * float(genes.get("glow_pulse_rate", 0.01)))
	var glow_intensity = glow_strength * pulse

	var glow_color = Color.from_hsv(float(genes.get("hue_a", 0.0)), 0.6, 1.0, 1.0)

	# Draw several translucent circles for a faux blur
	if glow_intensity > 0.01:
		var a = clamp(glow_intensity * 0.35, 0.0, 0.45)
		draw_circle(center, radius * 1.25, Color(glow_color.r, glow_color.g, glow_color.b, a * 0.35))
		draw_circle(center, radius * 1.10, Color(glow_color.r, glow_color.g, glow_color.b, a * 0.55))
		draw_circle(center, radius * 0.95, Color(glow_color.r, glow_color.g, glow_color.b, a * 0.80))

	

	# Draw petals
	# Draw petals (shape + edge from genotype)
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
				# balanced default
				length_mult *= 0.95
				width_mult *= 1.05
				base_in = 0.22
			"lance":
				length_mult *= 1.25
				width_mult *= 0.75
				base_in = 0.18
			"heart":
				# slightly wider + notch near base
				length_mult *= 0.95
				width_mult *= 1.20
				base_in = 0.24

		var base = center + dir * (radius * base_in)
		var tip = center + dir * (radius * length_mult)

		var w = radius * width_mult * 0.55
		w = clamp(w, radius * 0.10, radius * 0.95)

		# Control points: more forward for lance, more sideways for round/heart
		var forward = radius * length_mult * 0.30
		var base_push = radius * 0.10

		var left_ctrl = base + dir * (forward + base_push) + side * w
		var right_ctrl = base + dir * (forward + base_push) - side * w

		# Heart adds a little base notch by pulling controls inward near base
		if leaf_shape == "heart":
			left_ctrl -= side * (w * 0.20)
			right_ctrl += side * (w * 0.20)

		var pts = build_petal_outline(base, tip, left_ctrl, right_ctrl, steps)
		if leaf_shape == "heart":
			width_mult *= 1.35


		if leaf_shape == "heart":
			var notch_depth = clamp(w * 0.35, 2.0, radius * 0.25)
			pts = apply_heart_notch(pts, base, tip, notch_depth)

		# leaf_edge modifies outline: serrated/lobed
		# Use vein_contrast as a convenient "how intense" driver for edge texture
		var edge_intensity = clamp(vein_contrast, 0.0, 1.0)
		pts = apply_edge_style(pts, base, tip, leaf_edge, edge_intensity)

		draw_colored_polygon(pts, base_color)

		# Veins (stronger when vein_contrast is high)
		if vein_contrast > 0.05:
			var vein_color = base_color.lerp(Color.WHITE, vein_contrast)
			draw_line(base, tip, vein_color, 1.2 + 1.2 * vein_contrast)

		# Extra: heart has two subtle side veins
		if leaf_shape == "heart" and vein_contrast > 0.15:
			var vein_color2 = base_color.lerp(Color.WHITE, vein_contrast * 0.8)
			draw_line(base, base + (tip - base) * 0.75 + side * (w * 0.25), vein_color2, 1.0)
			draw_line(base, base + (tip - base) * 0.75 - side * (w * 0.25), vein_color2, 1.0)

		# Veins (optional)
		if vein_contrast > 0.05:
			var vein_color = base_color.lerp(Color.WHITE, vein_contrast)
			draw_line(base, tip, vein_color, 1.5)


	# Draw flower center
	draw_circle(center, radius * 0.35, center_color)

	# Spot patterns
	match spot_pattern:
		"freckle":
			for i in range(8):
				var r = randf_range(radius * 0.2, radius * 0.8)
				var a = randf() * TAU
				draw_circle(center + Vector2.RIGHT.rotated(a) * r, 2.0, center_color)
		"polka":
			for i in range(petal_count):
				var a = TAU * float(i) / float(petal_count)
				var r = radius * 0.65
				draw_circle(center + Vector2.RIGHT.rotated(a) * r, 3.0, center_color)
		"ring":
			draw_circle(center, radius * 0.75, center_color)
		_:
			pass
			
func apply_heart_notch(points: PackedVector2Array, base: Vector2, tip: Vector2, depth: float) -> PackedVector2Array:
	# Inserts a small V-notch near the base to create a "heart" silhouette.
	# depth is in pixels-ish, scaled by your petal size.
	if points.size() < 8:
		return points

	var axis = (tip - base)
	var axis_len = axis.length()
	if axis_len <= 0.001:
		return points
	axis /= axis_len
	var normal = axis.rotated(PI / 2.0)

	# We'll inject points near where the outline passes close to the base.
	# Because the outline is: base->tip (left edge) then tip->base (right edge),
	# the first couple points and last couple points are near base.
	var out := PackedVector2Array()

	# copy most points, but after 1st point add a notch, and before last point add a notch
	for idx in range(points.size()):
		out.append(points[idx])

		# After first point, insert the inward notch point
		if idx == 1:
			out.append(base + axis * (depth * 0.35) - normal * (depth * 0.60))
			out.append(base + axis * (depth * 0.35) + normal * (depth * 0.60))

		# Before the last point, insert notch again (mirrors the closure nicely)
		if idx == points.size() - 3:
			out.append(base + axis * (depth * 0.35) + normal * (depth * 0.60))
			out.append(base + axis * (depth * 0.35) - normal * (depth * 0.60))

	return out



func short_id() -> String:
	return str(get_instance_id() % 100000)

func species_tag() -> String:
	return str(species_id % 1000)

func other_species_tag(other) -> String:
	if other != null and "species_id" in other:
		return str(int(other.species_id) % 1000)
	return "?"
	
func gene_card_short() -> String:
	# One-liner-ish summary for console
	var pc = int(genes.get("petal_count", -1))
	var fs = float(genes.get("flower_size", 1.0))
	var pl = float(genes.get("petal_length", 0.0))
	var pw = float(genes.get("petal_width", 0.0))
	var leaf_shape = str(genes.get("leaf_shape", "?"))
	var leaf_edge = str(genes.get("leaf_edge", "?"))
	var spot = str(genes.get("spot_pattern", "?"))
	var veins = float(genes.get("vein_contrast", 0.0))
	var toxic = bool(genes.get("toxic", false))
	var scent = str(genes.get("scent", "none"))
	var inv = bool(genes.get("inv_glow_lock", false))
	var glow = float(genes.get("glow_max", 0.0))

	return "sp=%s petals=%d (L%.2f W%.2f) flower=%.2f leaf=%s/%s spots=%s veins=%.2f glow=%.2f toxic=%s scent=%s inv_lock=%s" % [
		species_tag(),
		pc, pl, pw, fs,
		leaf_shape, leaf_edge,
		spot, veins,
		glow,
		str(toxic),
		scent,
		str(inv)
	]
	
func _input_event(viewport: Viewport, event: InputEvent, shape_idx: int) -> void:
	if event is InputEventMouseButton and event.button_index == MOUSE_BUTTON_LEFT and event.pressed:
		emit_signal("cell_selected", self)




func gene_card_multiline() -> String:
	# More readable for label (multi-line)
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




func log_mating_event(other, dist: float, allowed: bool, reason: String, baby = null) -> void:
	var a_id = short_id()
	var b_id = str(other.get_instance_id() % 100000)
	var a_sp = species_tag()
	var b_sp = other_species_tag(other)

	var msg = "[%s] A#%s(sp=%s) ↔ B#%s(sp=%s) dist=%.3f allowed=%s reason=%s" % [
		Time.get_time_string_from_system(),
		a_id, a_sp,
		b_id, b_sp,
		dist,
		str(allowed),
		reason
	]

	if baby != null:
		var baby_id = str(baby.get_instance_id() % 100000)
		var baby_sp = "?"
		if "species_id" in baby:
			baby_sp = str(int(baby.species_id) % 1000)

		# Add a little genetics readout without spamming too hard
		var pc = int(baby.genes.get("petal_count", -1)) if "genes" in baby else -1
		var pat = str(baby.genes.get("spot_pattern", "?")) if "genes" in baby else "?"
		var glow = float(baby.genes.get("glow_max", 0.0)) if "genes" in baby else 0.0
		msg += "  -> baby#%s(sp=%s) petals=%s pattern=%s glow=%.2f" % [baby_id, baby_sp, str(pc), pat, glow]

	# print(msg)

# ============================================================
# SPECIES
# ============================================================

func compute_species_id() -> int:
	var qh = 12.0
	var qs = 6.0
	var qv = 6.0

	var ha = int(float(genes["hue_a"]) * qh)
	var sa = int(float(genes["sat_a"]) * qs)
	var va = int(float(genes["val_a"]) * qv)

	var hb = int(float(genes["hue_b"]) * qh)
	var sb = int(float(genes["sat_b"]) * qs)
	var vb = int(float(genes["val_b"]) * qv)

	return (ha << 0) ^ (sa << 4) ^ (va << 8) ^ (hb << 12) ^ (sb << 16) ^ (vb << 20)

func species_distance(a: Dictionary, b: Dictionary) -> float:
	var d_ha = hue_distance(float(a["hue_a"]), float(b["hue_a"]))
	var d_hb = hue_distance(float(a["hue_b"]), float(b["hue_b"]))

	var d_sa = abs(float(a["sat_a"]) - float(b["sat_a"]))
	var d_va = abs(float(a["val_a"]) - float(b["val_a"]))
	var d_sb = abs(float(a["sat_b"]) - float(b["sat_b"]))
	var d_vb = abs(float(a["val_b"]) - float(b["val_b"]))

	# Hue is most important; sat/val lightly contribute
	return (d_ha + d_hb) * 0.45 + (d_sa + d_va + d_sb + d_vb) * 0.10

# ============================================================
# MOVEMENT
# ============================================================

func _physics_process(delta):
	if dead:
		return

	age += delta
	if age >= lifespan:
		die()
		return

	velocity = wander_dir * float(genes["speed"])
	position += velocity * delta

	keep_in_bounds()
	update_glow(delta)
	queue_redraw()


func _on_wander_timer_timeout():
	var jitter = Vector2(randf_range(-1.0, 1.0), randf_range(-1.0, 1.0)).normalized()
	var keep = float(genes["wander_keep"])
	wander_dir = (wander_dir * keep + jitter * (1.0 - keep)).normalized()
	nudge_toward_others()

func nudge_toward_others():
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
		var s = float(genes["social"]) * float(genes["social_strength"])
		wander_dir = (wander_dir * (1.0 - s) + to_other * s).normalized()

# ============================================================
# GLOW
# ============================================================

func update_glow(delta):
	var fertile_start = float(genes.get("fertile_start", 0.1))
	var fertile_end = float(genes.get("fertile_end", 0.8))
	var fertile = age >= lifespan * fertile_start and age <= lifespan * fertile_end

	var target = float(genes["glow_max"]) if can_mate and fertile and not dead else 0.0

	glow_strength = lerp(glow_strength, target, float(genes["glow_ramp"]) * delta)

	var pulse = 0.5 + 0.5 * sin(Time.get_ticks_msec() * float(genes["glow_pulse_rate"]))
	var intensity = glow_strength * pulse

	var glow_color = Color.from_hsv(float(genes["hue_a"]), 0.6, 1.0, 1.0)
	self_modulate = Color(1, 1, 1, 1).lerp(glow_color, intensity)
	queue_redraw() # keep the pulse updating visually if you're drawing the flower


# ============================================================
# MATING
# ============================================================

func _on_area_entered(other):
	if other == self or not other.is_in_group("cells"):
		return

	# Only one side logs/initiates to avoid duplicate lines
	if get_instance_id() > other.get_instance_id():
		return

	var dist = species_distance(genes, other.genes)

	if dead:
		log_mating_event(other, dist, false, "dead")
		return

	if not can_mate:
		log_mating_event(other, dist, false, "cooldown_self")
		return

	if "can_mate" in other and not other.can_mate:
		log_mating_event(other, dist, false, "cooldown_other")
		return

	var fertile_start = float(genes.get("fertile_start", 0.1))
	var fertile_end = float(genes.get("fertile_end", 0.8))

	if age < lifespan * fertile_start:
		log_mating_event(other, dist, false, "too_young")
		return

	if age > lifespan * fertile_end:
		log_mating_event(other, dist, false, "too_old")
		return

	var my_thresh = float(genes.get("species_threshold", 0.5))
	if dist > my_thresh:
		log_mating_event(other, dist, false, "species_block")
		return

	log_mating_event(other, dist, true, "ok")
	call_deferred("mate_with_deferred", other, dist)

func mate_with_deferred(partner: Area2D, dist: float) -> void:
	if dead:
		return
	if not is_instance_valid(partner):
		return
	if "dead" in partner and partner.dead:
		return

	mate_with(partner, dist)

func mate_with(partner, dist: float = -1.0) -> void:
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
	# print("[%s] spawn #%s %s" % [Time.get_time_string_from_system(), short_id(), gene_card_short()])

	baby.can_mate = false

	if dist < 0.0:
		dist = species_distance(genes, partner.genes)
	log_mating_event(partner, dist, true, "mated", baby)

	var cooldown = float(genes.get("mate_cooldown", 1.5))

	var self_ref = weakref(self)
	var partner_ref = weakref(partner)

	get_tree().create_timer(cooldown).timeout.connect(func ():
		var s = self_ref.get_ref()
		if s != null and is_instance_valid(s) and not s.dead:
			s.can_mate = true

		var p = partner_ref.get_ref()
		if p != null and is_instance_valid(p) and not p.dead:
			p.can_mate = true
	)

# ============================================================
# DEATH
# ============================================================

func die():
	dead = true
	can_mate = false
	set_physics_process(false)
	wander_timer.stop()

	var tween = create_tween()
	tween.tween_property(sprite, "modulate:a", 0.0, 0.6)
	tween.finished.connect(queue_free)

# ============================================================
# BOUNDS
# ============================================================

func keep_in_bounds():
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

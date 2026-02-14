extends Area2D
##
## Cell / Flower Creature with:
## - Per-instance gradient coloring + size
## - Simple wandering + social nudging
## - Mating + recombination with linked loci on 2 chromosomes
## - Hotspots + inversion that suppresses recombination in a segment
## - Drawn procedural flower petals (including heart petals) + edge styles
## - Optional genetics label overlay
## - Capsule collision auto-scales to match flower genes (size + petals)
##
## Copy/paste ready. ✅
##
## Node requirements (children):
##   - Sprite2D named "Sprite2D"   (can be hidden; used only for optional gradient storage)
##   - Timer   named "WanderTimer"
##   - Label   named "SpeciesLabel"
##   - CollisionShape2D named "CollisionShape2D" with a CapsuleShape2D
##
## Inspector checklist is at the end of this message.
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

const CHR1_LOCI := [
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
	{ "name": "petal_count", "pos": 0.08 },

	# gradient genes (linked chunks)
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

	# pattern + glow + toxic live here (lockable by inversion)
	{ "name": "spot_pattern", "pos": 0.60 },
	{ "name": "vein_contrast", "pos": 0.66 },

	{ "name": "glow_max", "pos": 0.72 },
	{ "name": "glow_pulse_rate", "pos": 0.76 },
	{ "name": "glow_ramp", "pos": 0.79 },

	{ "name": "toxic", "pos": 0.86 },
	{ "name": "scent", "pos": 0.92 },
]

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

const INV_GLOW_LOCK := { "chr": 2, "start": 0.55, "end": 0.90 }

# ============================================================
# GENE SCHEMA
# ============================================================

const GENE_SCHEMA := {
	"hue_a": { "type": "hue",   "min": 0.0, "max": 1.0, "mut_amount": 0.10 },
	"sat_a": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },
	"val_a": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },

	"hue_b": { "type": "hue",   "min": 0.0, "max": 1.0, "mut_amount": 0.10 },
	"sat_b": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },
	"val_b": { "type": "float", "min": 0.55, "max": 1.0, "mut_amount": 0.12 },

	"mut_rate": { "type": "float", "min": 0.0, "max": 0.35, "mut_amount": 0.08 },
	"lifespan":  { "type": "float", "min": 6.0, "max": 120.0, "mut_amount": 6.0 },

	"speed": { "type": "float", "min": 40.0, "max": 360.0, "mut_amount": 25.0 },

	"social":          { "type": "float", "min": 0.0, "max": 2.0, "mut_amount": 0.25 },
	"social_strength": { "type": "float", "min": 0.0, "max": 0.5, "mut_amount": 0.05 },

	"wander_rate": { "type": "float", "min": 0.05, "max": 2.5, "mut_amount": 0.15 },
	"wander_keep": { "type": "float", "min": 0.0, "max": 1.0, "mut_amount": 0.10 },

	"mate_cooldown": { "type": "float", "min": 0.2, "max": 1.0, "mut_amount": 0.5 },
	"fertile_start": { "type": "float", "min": 0.0, "max": 0.2, "mut_amount": 0.08 },
	"fertile_end":   { "type": "float", "min": 0.6, "max": 1.0, "mut_amount": 0.08 },

	"species_threshold": { "type": "float", "min": 0.4, "max": 0.9, "mut_amount": 0.08 },

	"glow_max":        { "type": "float", "min": 1.0, "max": 2.0, "mut_amount": 0.25 },
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

var genes := {}

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

	# We draw the flower in _draw(); keep sprite hidden.
	if sprite:
		sprite.visible = false

	queue_redraw()

	# Make gradient unique per instance (if you happen to keep a GradientTexture2D on Sprite2D).
	var tex := sprite.texture as GradientTexture2D
	if tex != null:
		tex = tex.duplicate(true)
		if tex.gradient != null:
			tex.gradient = tex.gradient.duplicate(true)
		sprite.texture = tex

	if genes.is_empty():
		genes = roll_random_genes()

	normalize_fertility_window(genes)
	lifespan = float(genes.get("lifespan", 20.0))

	apply_genes()

	if wander_timer:
		wander_timer.wait_time = float(genes.get("wander_rate", 1.0))
		if not wander_timer.timeout.is_connected(_on_wander_timer_timeout):
			wander_timer.timeout.connect(_on_wander_timer_timeout)

	if not area_entered.is_connected(_on_area_entered):
		area_entered.connect(_on_area_entered)

	_on_wander_timer_timeout()

# ============================================================
# GENETICS LABEL DISPLAY
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
	lines.append("inv_glow_lock: %s" % str(bool(genes.get("inv_glow_lock", false))))
	return "\n".join(lines)

# ============================================================
# GENE SYSTEM
# ============================================================

func roll_random_genes() -> Dictionary:
	var g := {}
	for key in GENE_SCHEMA:
		var s = GENE_SCHEMA[key]
		match s["type"]:
			"float":
				g[key] = randf_range(float(s["min"]), float(s["max"]))
			"hue":
				g[key] = randf()
			"int":
				g[key] = randi_range(int(s["min"]), int(s["max"]))
			"bool":
				g[key] = randf() < 0.5
			"enum":
				var vals: Array = s["values"]
				g[key] = vals[randi() % vals.size()]
			_:
				pass

	if not g.has("inv_glow_lock"):
		g["inv_glow_lock"] = randf() < 0.25
	return g

func normalize_fertility_window(g: Dictionary) -> void:
	var fs: float = float(g.get("fertile_start", 0.1))
	var fe: float = float(g.get("fertile_end", 0.8))

	fs = clamp(fs, 0.0, 0.95)
	fe = clamp(fe, 0.05, 1.0)

	if fs >= fe:
		fs = max(0.0, fe - 0.1)

	g["fertile_start"] = fs
	g["fertile_end"] = fe

func choose_parent_any(a, b):
	return a if randf() < 0.5 else b

func mutate_hue(h: float, mut_rate: float, amount: float) -> float:
	var out := h
	if randf() < mut_rate:
		out = fposmod(out + randf_range(-amount, amount), 1.0)
	return out

func hue_distance(a: float, b: float) -> float:
	var d = abs(a - b)
	return min(d, 1.0 - d)

func mutate_float(v: float, mut_rate: float, lo: float, hi: float, amount: float) -> float:
	var out := v
	if randf() < mut_rate:
		out += randf_range(-amount, amount)
	return clamp(out, lo, hi)

func mutate_int(v: int, mut_rate: float, lo: int, hi: int, step_amount: int) -> int:
	var out := v
	if randf() < mut_rate:
		var step := randi_range(-step_amount, step_amount)
		if randf() < (mut_rate * 0.15):
			step += randi_range(-step_amount * 2, step_amount * 2)
		out += step
	return clamp(out, lo, hi)

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

# --- Recombination utilities (linked loci + lam + hotspots + inversions) ---

func poisson(lam: float) -> int:
	var L = exp(-lam)
	var k := 0
	var p := 1.0
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

func sample_cut_respecting(cfg: Dictionary, suppressed_spans: Array, max_tries: int = 24) -> float:
	for _i in range(max_tries):
		var c = sample_cut(cfg)
		if not cut_is_suppressed(c, suppressed_spans):
			return c
	return -1.0

func get_suppressed_spans_for_chr2(a: Dictionary, b: Dictionary) -> Array:
	var spans := []
	var inv_a = bool(a.get("inv_glow_lock", false))
	var inv_b = bool(b.get("inv_glow_lock", false))
	if inv_a != inv_b:
		spans.append([float(INV_GLOW_LOCK["start"]), float(INV_GLOW_LOCK["end"])])
	return spans

func recombine_linked(a: Dictionary, b: Dictionary, loci: Array, cfg: Dictionary, suppressed_spans: Array) -> Dictionary:
	var out := {}

	var lam = float(cfg.get("lam", 1.0))
	var n = poisson(lam)

	var cuts := []
	for _i in range(n):
		var cut = sample_cut_respecting(cfg, suppressed_spans)
		if cut >= 0.0:
			cuts.append(cut)
	cuts.sort()

	var flip := false
	var ci := 0

	if randf() < 0.5:
		flip = true

	for locus in loci:
		var pos := float(locus["pos"])
		var name := String(locus["name"])

		while ci < cuts.size() and pos > float(cuts[ci]):
			flip = !flip
			ci += 1

		out[name] = b.get(name) if flip else a.get(name)

	return out

func recombine_genes(a: Dictionary, b: Dictionary) -> Dictionary:
	var child := {}
	var cmut = lerp(float(a.get("mut_rate", 0.1)), float(b.get("mut_rate", 0.1)), randf())

	var chr1 = recombine_linked(a, b, CHR1_LOCI, CHR_CFG[1], [])
	var suppressed2 = get_suppressed_spans_for_chr2(a, b)
	var chr2 = recombine_linked(a, b, CHR2_LOCI, CHR_CFG[2], suppressed2)

	for k in chr1.keys():
		child[k] = chr1[k]
	for k in chr2.keys():
		child[k] = chr2[k]

	for key in GENE_SCHEMA:
		if child.has(key):
			continue
		var s = GENE_SCHEMA[key]
		match s["type"]:
			"float", "hue", "int", "bool", "enum":
				child[key] = choose_parent_any(a.get(key), b.get(key))
			_:
				pass

	for key in child.keys():
		child[key] = mutate_value_by_schema(key, child[key], cmut)

	normalize_fertility_window(child)
	return child

# ============================================================
# APPLY GENES (visual + derived state)
# ============================================================

func apply_genes() -> void:
	# Update gradient (optional)
	var tex := sprite.texture as GradientTexture2D
	if tex != null and tex.gradient != null:
		var ca = Color.from_hsv(float(genes.get("hue_a", 0.0)), float(genes.get("sat_a", 1.0)), float(genes.get("val_a", 1.0)), 1.0)
		var cb = Color.from_hsv(float(genes.get("hue_b", 0.0)), float(genes.get("sat_b", 1.0)), float(genes.get("val_b", 1.0)), 1.0)
		tex.gradient.set_color(0, ca)
		tex.gradient.set_color(1, cb)

	# Update species id + label
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

		var tox = bool(genes.get("toxic", false))
		species_label.modulate = Color(1, 0.9, 0.9, 1) if tox else Color(1, 1, 1, 1)

	# IMPORTANT: collision scales with genetics
	update_collision_shape_from_genes()

	queue_redraw()

# ============================================================
# COLLISION SHAPE: Capsule scales with flower size + petals
# ============================================================

func update_collision_shape_from_genes() -> void:
	if collision_shape == null:
		return
	if collision_shape.shape == null:
		return

	var cap := collision_shape.shape as CapsuleShape2D
	if cap == null:
		# You can switch to CircleShape2D if you want, but capsule is best for "bulby plant".
		return

	var flower_size: float = float(genes.get("flower_size", 1.0))
	var petal_length: float = float(genes.get("petal_length", 1.0))
	var petal_width: float = float(genes.get("petal_width", 1.0))

	# Same base radius used in drawing: radius = 20 * flower_size
	var base_radius: float = 20.0 * flower_size

	# Capsule radius: "body width" of the plant (includes petals a bit)
	var r: float = base_radius * (0.42 + 0.22 * clamp(petal_width, 0.2, 2.0) + 0.06 * clamp(petal_length, 0.5, 3.0))

	# Capsule reach: petals extend outward, so increase half-length with petal_length
	var half_len: float = base_radius * (0.45 + 0.65 * clamp(petal_length, 0.5, 3.0))

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

		var amp = min(intensity * side_weight * 2.0, 3.0)

		out[i] = p + normal * (jitter * amp)

	return out

# Heart silhouette oriented base -> tip, built from bezier segments (no nested functions).
## HEART PETAL OUTLINE (stable "petal-heart")
##
## Builds a heart-shaped petal silhouette oriented from `base` -> `tip`.
## Returns points in a non-self-intersecting order:
## notch -> left lobe -> tip -> right lobe -> back to notch.
func build_heart_outline(base: Vector2, tip: Vector2, width: float, steps: int) -> PackedVector2Array:
	var axis := (tip - base)
	var axis_len := axis.length()
	if axis_len <= 0.001:
		return PackedVector2Array()

	var forward := axis / axis_len
	var right := forward.rotated(PI / 2.0)

	# Shape tuning (feel free to tweak)
	var notch_depth: float = axis_len * 0.18
	var lobe_height: float = axis_len * 0.35
	var tip_taper: float   = 0.55

	width = max(width, axis_len * 0.05)

	var pts := PackedVector2Array()

	# Points that define the silhouette
	var notch: Vector2 = base + forward * (notch_depth * 0.55)

	var notch_left: Vector2  = base + forward * (notch_depth * 0.25) + right * (width * 0.20)
	var notch_right: Vector2 = base + forward * (notch_depth * 0.25) - right * (width * 0.20)

	var lobe_left: Vector2  = base + forward * lobe_height + right * (width * 0.55)
	var lobe_right: Vector2 = base + forward * lobe_height - right * (width * 0.55)

	var tip_point: Vector2 = tip

	# Segment counts
	var seg: int = max(4, int(steps / 6))
	var seg2: int = max(6, int(steps / 3))

	# notch -> lobe_left
	for i in range(seg + 1):
		var t: float = float(i) / float(seg)
		pts.append(quad_bezier(notch, notch_left, lobe_left, t))

	# lobe_left -> tip (with taper squeeze)
	for i in range(seg2 + 1):
		var t: float = float(i) / float(seg2)
		var ctrl: Vector2 = lobe_left.lerp(base + forward * (axis_len * 0.75), 0.55)
		var p: Vector2 = quad_bezier(lobe_left, ctrl, tip_point, t)

		var along: float = (p - base).dot(forward)
		var u: float = clamp(along / axis_len, 0.0, 1.0)
		var squeeze: float = lerp(1.0, tip_taper, pow(u, 1.8))
		var lateral: float = (p - base).dot(right)
		p = base + forward * along + right * (lateral * squeeze)

		pts.append(p)

	# tip -> lobe_right
	for i in range(seg2 + 1):
		var t: float = float(i) / float(seg2)
		var ctrl: Vector2 = lobe_right.lerp(base + forward * (axis_len * 0.75), 0.55)
		var p: Vector2 = quad_bezier(tip_point, ctrl, lobe_right, t)

		var along: float = (p - base).dot(forward)
		var u: float = clamp(along / axis_len, 0.0, 1.0)
		var squeeze: float = lerp(1.0, tip_taper, pow(u, 1.8))
		var lateral: float = (p - base).dot(right)
		p = base + forward * along + right * (lateral * squeeze)

		pts.append(p)

	# lobe_right -> notch (THIS DIRECTION MATTERS)
	for i in range(seg + 1):
		var t: float = float(i) / float(seg)
		pts.append(quad_bezier(lobe_right, notch_right, notch, t))

	return pts

## Removes near-duplicate points (and optional closing duplicate).
func _clean_polygon(points: PackedVector2Array, eps: float = 0.5) -> PackedVector2Array:
	var out := PackedVector2Array()
	if points.size() < 3:
		return out

	var last: Vector2 = points[0]
	out.append(last)

	for i in range(1, points.size()):
		var p: Vector2 = points[i]
		if p.distance_to(last) > eps:
			out.append(p)
			last = p

	# Remove closing duplicate if present
	if out.size() >= 2 and out[0].distance_to(out[out.size() - 1]) <= eps:
		out.remove_at(out.size() - 1)

	return out


## Returns true if polygon is drawable (triangulates successfully).
func _is_triangulatable(points: PackedVector2Array) -> bool:
	if points.size() < 3:
		return false
	var indices: PackedInt32Array = Geometry2D.triangulate_polygon(points)
	return indices.size() >= 3


## Draw polygon safely: if triangulation fails, draw an outline instead (no error spam).
func _safe_draw_colored_polygon(points: PackedVector2Array, color: Color, outline_width: float = 1.0) -> void:
	var pts := _clean_polygon(points, 0.5)
	if pts.size() < 3:
		return

	if _is_triangulatable(pts):
		draw_colored_polygon(pts, color)
	else:
		# Fallback: outline only (prevents "triangulation failed" spam)
		# Close the loop visually
		var loop := PackedVector2Array(pts)
		loop.append(pts[0])
		draw_polyline(loop, color, outline_width, true)


# ============================================================
# DRAW
# ============================================================

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

	# --- Halo (soft glow) ---
	var fertile_start = float(genes.get("fertile_start", 0.1))
	var fertile_end = float(genes.get("fertile_end", 0.8))
	var fertile = age >= lifespan * fertile_start and age <= lifespan * fertile_end

	var pulse = 0.5 + 0.5 * sin(Time.get_ticks_msec() * float(genes.get("glow_pulse_rate", 0.01)))
	var glow_intensity = glow_strength * pulse
	var glow_color = Color.from_hsv(float(genes.get("hue_a", 0.0)), 0.6, 1.0, 1.0)

	if glow_intensity > 0.01 and can_mate and fertile and not dead:
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

		_safe_draw_colored_polygon(pts, base_color, 1.0)


		if vein_contrast > 0.05:
			var vein_color = base_color.lerp(Color.WHITE, vein_contrast)
			draw_line(base, tip, vein_color, 1.2 + 1.2 * vein_contrast)

			if leaf_shape == "heart" and vein_contrast > 0.15:
				var vein_color2 = base_color.lerp(Color.WHITE, vein_contrast * 0.8)
				draw_line(base, base + (tip - base) * 0.75 + side * (w * 0.25), vein_color2, 1.0)
				draw_line(base, base + (tip - base) * 0.75 - side * (w * 0.25), vein_color2, 1.0)

	# --- Center disk ---
	draw_circle(center, radius * 0.35, center_color)

	# --- Spots ---
	match spot_pattern:
		"freckle":
			for _j in range(8):
				var rr = rng.randf_range(radius * 0.2, radius * 0.8)
				var aa = rng.randf() * TAU
				draw_circle(center + Vector2.RIGHT.rotated(aa) * rr, 2.0, center_color)
		"polka":
			for j in range(petal_count):
				var aa = TAU * float(j) / float(petal_count)
				draw_circle(center + Vector2.RIGHT.rotated(aa) * (radius * 0.65), 3.0, center_color)
		"ring":
			draw_circle(center, radius * 0.75, center_color)
		_:
			pass

# ============================================================
# INPUT (selection)
# ============================================================

func _input_event(_viewport: Viewport, event: InputEvent, _shape_idx: int) -> void:
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

func _on_area_entered(other) -> void:
	if other == self or not other.is_in_group("cells"):
		return

	# only one side initiates to avoid duplicates
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
	baby.can_mate = false

	var cooldown: float = float(genes.get("mate_cooldown", 1.5))

	var self_ref = weakref(self)
	var partner_ref = weakref(partner)

	var t := get_tree().create_timer(cooldown)

	# IMPORTANT: store lambda in a variable (fixes "Standalone lambdas cannot be accessed")
	var cb := func() -> void:
		var s = self_ref.get_ref()
		if s != null and is_instance_valid(s) and not s.dead:
			s.can_mate = true

		var p = partner_ref.get_ref()
		if p != null and is_instance_valid(p) and not p.dead:
			p.can_mate = true

	t.timeout.connect(cb)

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
# ID HELPERS (optional)
# ============================================================

func short_id() -> String:
	return str(get_instance_id() % 100000)

func species_tag() -> String:
	return str(species_id % 1000)

extends Node2D

@export var cell_scene: PackedScene

@onready var spawn10_btn: Button = $UI/Panel/VBox/TopRow/Spawn10
@onready var show_genetics_toggle: CheckButton = $UI/Panel/VBox/SecondRow/ShowGenetics
@onready var inspector_mode: OptionButton = $UI/Panel/VBox/SecondRow/InspectorMode
@onready var genetics_text: RichTextLabel = $UI/Panel/VBox/GeneticsText
@onready var title_label: Label = $UI/Panel/VBox/Title

var show_genetics: bool = true
var selected_cell: Node = null

enum InspectorMode {
	PHENOTYPE,
	GENOTYPE,
	CHROMOSOMES,
	ALL
}

var mode: int = InspectorMode.ALL

func _ready() -> void:
	# Buttons / toggles
	spawn10_btn.pressed.connect(_on_spawn10_pressed)
	show_genetics_toggle.toggled.connect(_on_show_genetics_toggled)

	# OptionButton setup (if not already configured in editor)
	if inspector_mode.item_count == 0:
		inspector_mode.add_item("Phenotype", InspectorMode.PHENOTYPE)
		inspector_mode.add_item("Genotype", InspectorMode.GENOTYPE)
		inspector_mode.add_item("Chromosomes", InspectorMode.CHROMOSOMES)
		inspector_mode.add_item("All", InspectorMode.ALL)
	inspector_mode.item_selected.connect(_on_inspector_mode_selected)
	inspector_mode.select(InspectorMode.ALL)

	# Connect any cells that already exist
	for c in get_tree().get_nodes_in_group("cells"):
		_try_connect_cell(c)

	# Catch babies spawned at runtime
	get_tree().node_added.connect(_on_node_added)

	_set_inspector_text("Click a flower to inspect 🧬")

func _on_inspector_mode_selected(idx: int) -> void:
	mode = inspector_mode.get_item_id(idx)
	_refresh_selected()

func _on_node_added(n: Node) -> void:
	_try_connect_cell(n)

func _try_connect_cell(n: Node) -> void:
	if n == null:
		return
	if n.has_signal("cell_selected"):
		# Important: do NOT double-connect
		if not n.cell_selected.is_connected(set_selected_cell):
			n.cell_selected.connect(set_selected_cell)

func _on_spawn10_pressed() -> void:
	spawn_cells(10)

func _on_show_genetics_toggled(on: bool) -> void:
	show_genetics = on

	if not show_genetics:
		_set_inspector_text("Inspector hidden (toggle ON to inspect)")
	else:
		_set_inspector_text("Click a flower to inspect 🧬")

	# Tell cells whether they should show genetics in-label (optional)
	for c in get_tree().get_nodes_in_group("cells"):
		if c != null and c.has_method("set_genetics_visible"):
			c.set_genetics_visible(show_genetics)

	_refresh_selected()

func _set_inspector_text(s: String) -> void:
	if genetics_text == null:
		print("genetics_text is null; check node path")
		return
	genetics_text.clear()
	genetics_text.append_text(s)

func set_selected_cell(cell: Node) -> void:
	selected_cell = cell
	_refresh_selected()

func _refresh_selected() -> void:
	if not show_genetics:
		return

	if selected_cell == null:
		_set_inspector_text("No selection")
		return

	# header
	var header: String = "\nCell #%s | species %s\n\n" % [
		selected_cell.call("short_id") if selected_cell.has_method("short_id") else "?",
		selected_cell.call("species_tag") if selected_cell.has_method("species_tag") else "?"
	]

	var body: String = ""

	match mode:
		InspectorMode.PHENOTYPE:
			body = selected_cell.call("phenotype_card_multiline") if selected_cell.has_method("phenotype_card_multiline") else "(missing phenotype_card_multiline)"
		InspectorMode.GENOTYPE:
			body = selected_cell.call("genotype_card_multiline") if selected_cell.has_method("genotype_card_multiline") else "(missing genotype_card_multiline)"
		InspectorMode.CHROMOSOMES:
			body = selected_cell.call("chromosomes_visualizer_text") if selected_cell.has_method("chromosomes_visualizer_text") else "(missing chromosomes_visualizer_text)"
		InspectorMode.ALL:
			var parts: Array[String] = []
			if selected_cell.has_method("phenotype_card_multiline"):
				parts.append(selected_cell.call("phenotype_card_multiline"))
			if selected_cell.has_method("genotype_card_multiline"):
				parts.append("\n\n" + selected_cell.call("genotype_card_multiline"))
			if selected_cell.has_method("chromosomes_visualizer_text"):
				parts.append("\n\n" + selected_cell.call("chromosomes_visualizer_text"))
			body = "\n".join(parts)

	_set_inspector_text(header + body)

func spawn_cells(n: int) -> void:
	if cell_scene == null:
		push_warning("World: cell_scene not set")
		return

	var rect: Rect2 = get_viewport_rect()
	for i in range(n):
		var c: Node = cell_scene.instantiate()
		add_child(c)

		# Connect selection (one place only)
		_try_connect_cell(c)

		# Position
		if c is Node2D:
			(c as Node2D).position = Vector2(
				randf_range(rect.position.x + 60.0, rect.end.x - 260.0), # leave space for side panel
				randf_range(rect.position.y + 60.0, rect.end.y - 60.0)
			)

		# Give the cell a reference to world
		if c.has_method("set_world"):
			c.call("set_world", self)

		# Apply toggle behavior
		if c.has_method("set_genetics_visible"):
			c.call("set_genetics_visible", show_genetics)

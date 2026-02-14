extends Node2D

@export var cell_scene: PackedScene

@onready var spawn10_btn: Button = $UI/Panel/VBox/TopRow/Spawn10
@onready var show_genetics_toggle: CheckButton = $UI/Panel/VBox/SecondRow/ShowGenetics
@onready var genetics_text: RichTextLabel = $UI/Panel/VBox/GeneticsText
@onready var title_label: Label = $UI/Panel/VBox/Title

var show_genetics := true
var selected_cell: Node = null


func _ready() -> void:
	print("genetics_text = ", genetics_text)

	# 1) Connect UI
	if spawn10_btn and not spawn10_btn.pressed.is_connected(_on_spawn10_pressed):
		spawn10_btn.pressed.connect(_on_spawn10_pressed)

	if show_genetics_toggle and not show_genetics_toggle.toggled.is_connected(_on_show_genetics_toggled):
		show_genetics_toggle.toggled.connect(_on_show_genetics_toggled)

	# Keep our local flag in sync with the toggle's current state (if you want)
	if show_genetics_toggle:
		show_genetics = show_genetics_toggle.button_pressed

	# 2) Connect existing cells already in the tree
	for c in get_tree().get_nodes_in_group("cells"):
		_try_connect_cell(c)

	# 3) Auto-connect any cells added later (babies, spawns)
	if not get_tree().node_added.is_connected(_on_node_added):
		get_tree().node_added.connect(_on_node_added)

	_set_inspector_text("Click a flower to inspect 🧬")


func _on_node_added(n: Node) -> void:
	# Babies get added during runtime; catch them here.
	_try_connect_cell(n)


func _try_connect_cell(n: Node) -> void:
	if n == null:
		return

	# Prefer signal existence over group membership because group may be set in _ready later.
	if n.has_signal("cell_selected"):
		if not n.cell_selected.is_connected(set_selected_cell):
			n.cell_selected.connect(set_selected_cell)

		# Apply current toggle behavior to any newly-added cell
		if n.has_method("set_genetics_visible"):
			n.set_genetics_visible(show_genetics)


func _on_spawn10_pressed() -> void:
	spawn_cells(10)


func _on_show_genetics_toggled(on: bool) -> void:
	show_genetics = on

	if not show_genetics:
		_set_inspector_text("Genetics hidden (toggle ON to inspect)")
	else:
		_set_inspector_text("Click a flower to inspect 🧬")

	# Tell all existing cells to update their label behavior
	for c in get_tree().get_nodes_in_group("cells"):
		if c != null and c.has_method("set_genetics_visible"):
			c.set_genetics_visible(show_genetics)


func _set_inspector_text(s: String) -> void:
	if genetics_text == null:
		print("genetics_text is null; check node path")
		return
	genetics_text.clear()
	genetics_text.append_text(s)


func set_selected_cell(cell: Node) -> void:
	selected_cell = cell

	if not show_genetics:
		return

	if selected_cell == null:
		_set_inspector_text("No selection")
		return

	# Prefer a method on the cell that returns a nice multiline card
	if selected_cell.has_method("gene_card_multiline"):
		var header := "\n\nCell #%s | species %s\n\n" % [
			selected_cell.call("short_id"),
			selected_cell.call("species_tag")
		]
		_set_inspector_text(header + selected_cell.call("gene_card_multiline"))
	else:
		_set_inspector_text("Selected cell has no gene_card_multiline()")


func spawn_cells(n: int) -> void:
	if cell_scene == null:
		push_warning("World: cell_scene not set")
		return

	var rect := get_viewport_rect()

	for i in range(n):
		var c = cell_scene.instantiate()
		add_child(c) # node_added will connect signals + apply genetics visibility

		if c is Node2D:
			c.position = Vector2(
				randf_range(rect.position.x + 60.0, rect.end.x - 260.0), # leave space for side panel
				randf_range(rect.position.y + 60.0, rect.end.y - 60.0)
			)

		# Optional: give the cell a reference to world
		if c.has_method("set_world"):
			c.set_world(self)

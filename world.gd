extends Node2D

@export var cell_scene: PackedScene

@onready var spawn10_btn: Button = $UI/Panel/VBox/TopRow/Spawn10
@onready var show_genetics_toggle: CheckButton = $UI/Panel/VBox/SecondRow/ShowGenetics
@onready var genetics_text: RichTextLabel = $UI/Panel/VBox/GeneticsText
@onready var title_label: Label = $UI/Panel/VBox/Title

var show_genetics := true
var selected_cell: Node = null

func _ready() -> void:
	# print("WORLD READY RUNNING")
	print("genetics_text = ", genetics_text)

	# Connect to any cells that already exist in the scene
	for c in get_tree().get_nodes_in_group("cells"):
		if c != null and c.has_signal("cell_selected"):
			if not c.cell_selected.is_connected(set_selected_cell):
				c.cell_selected.connect(set_selected_cell)
				# print("Connected existing cell_selected for ", c)

	get_tree().node_added.connect(_on_node_added)

	# connect any cells that already exist
	for c in get_tree().get_nodes_in_group("cells"):
		_try_connect_cell(c)
		
	_set_inspector_text("Click a flower to inspect 🧬")
	# print("spawn10_btn = ", spawn10_btn)
	spawn10_btn.pressed.connect(_on_spawn10_pressed)

func _on_node_added(n: Node) -> void:
	# babies get added during runtime; catch them here
	_try_connect_cell(n)

func _try_connect_cell(n: Node) -> void:
	if n == null:
		return
	# Prefer signal existence over group membership because group may be set in _ready later
	if n.has_signal("cell_selected"):
		if not n.cell_selected.is_connected(set_selected_cell):
			n.cell_selected.connect(set_selected_cell)
			# optional debug:
			# print("World connected cell_selected for ", n)


func _on_spawn10_pressed() -> void:
	# print("SPAWN 10 PRESSED")
	spawn_cells(10)

func _on_show_genetics_toggled(on: bool) -> void:
	show_genetics = on
	if not show_genetics:
		_set_inspector_text("Genetics hidden (toggle ON to inspect)")
	else:
		_set_inspector_text("Click a flower to inspect 🧬")
		


	# Also tell cells whether they should show genetics in-label (if you still use that)
	for c in get_tree().get_nodes_in_group("cells"):
		if c != null and c.has_method("set_genetics_visible"):
			c.set_genetics_visible(show_genetics)
			
	# print("genetics_text = ", genetics_text)

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
		var header = "\n\nCell #%s | species %s\n\n" % [
			selected_cell.call("short_id"),
			selected_cell.call("species_tag")
		]
		_set_inspector_text(header + selected_cell.call("gene_card_multiline"))
	else:
		_set_inspector_text("Selected cell has no gene_card_multiline()")

func spawn_cells(n: int) -> void:
	# print("spawn_cells called n=", n, " cell_scene=", cell_scene)

	if cell_scene == null:
		push_warning("World: cell_scene not set")
		return

	var rect := get_viewport_rect()
	for i in range(n):
		var c = cell_scene.instantiate()
		add_child(c)
		# Connect click signal to inspector
		if c.has_signal("cell_selected"):
			c.cell_selected.connect(set_selected_cell)

		# print("Connected cell_selected for ", c)
		if c.has_signal("cell_selected"):
			c.cell_selected.connect(set_selected_cell)
			# print("Connected spawned cell_selected for ", c)


		if c is Node2D:
			c.position = Vector2(
				randf_range(rect.position.x + 60.0, rect.end.x - 260.0), # leave space for side panel
				randf_range(rect.position.y + 60.0, rect.end.y - 60.0)
			)

		# Give the cell a reference to world so it can report clicks
		if c.has_method("set_world"):
			c.set_world(self)

		# Optional: apply toggle behavior to new cells too
		if c.has_method("set_genetics_visible"):
			c.set_genetics_visible(show_genetics)

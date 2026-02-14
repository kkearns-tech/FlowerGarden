extends Node2D

@onready var sprite: Sprite2D = $Sprite2D

var selected = false

func _ready():
	randomize()
	add_to_group("cells")
	set_selected(false)

func _unhandled_input(event):
	if event is InputEventMouseButton and event.pressed:
		if event.button_index == MOUSE_BUTTON_LEFT:
			if _was_clicked(event.position):
				get_tree().call_group("cells", "set_selected", false)
				set_selected(true)

		if event.button_index == MOUSE_BUTTON_RIGHT:
			if selected and _was_clicked(event.position):
				split()

	if event.is_action_pressed("ui_accept"):
		if selected:
			split()

func set_selected(value: bool):
	selected = value

	if selected:
		sprite.scale = Vector2(1.1, 1.1)
		sprite.modulate = Color(1.2, 1.2, 1.2, 1.0)
	else:
		sprite.scale = Vector2(1.0, 1.0)
		sprite.modulate = Color(1.0, 1.0, 1.0, 1.0)

func _was_clicked(screen_pos: Vector2) -> bool:
	if sprite.texture == null:
		return false

	var local_pos = sprite.to_local(screen_pos)
	var rect = sprite.get_rect()  # local rect centered on the sprite
	return rect.has_point(local_pos)

func split():
	var baby = duplicate()
	baby.position += Vector2(50, 50)
	get_parent().add_child(baby)

	baby.add_to_group("cells")
	baby.set_selected(false)

	var baby_sprite = baby.get_node("Sprite2D")
	baby_sprite.modulate = Color(randf(), randf(), randf(), 1.0)

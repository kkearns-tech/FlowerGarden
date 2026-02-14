
# 🌸 FlowerGarden

*A cozy chaotic evolutionary plant simulation built in Godot.*

## What Is This?

FlowerGarden is an experimental simulation where stylized, generative flowers move, age, glow, mate, mutate, and evolve over time.

Each plant carries a genome made of linked chromosomes. Traits like petal shape, glow intensity, toxicity, scent, and even structural inversions are inherited through recombination and mutation. Over generations, distinct visual lineages emerge organically.

It’s part toy, part art experiment, part miniature genetics lab.

---

## 🌿 Core Features

### 🧬 Linked Chromosomes

* Two chromosomes with ordered loci
* Poisson-distributed crossovers
* Recombination hotspots
* Inversion-based recombination suppression

Traits travel in chunks instead of random noise, allowing recognizable lineages to form.

---

### 🌸 Expressive Morphology

Plants vary in:

* Petal count
* Petal length & width
* Flower size
* Leaf silhouette (round, oval, lance, heart)
* Edge styles (smooth, serrated, lobed)
* Spot patterns (freckle, polka, ring)
* Vein contrast
* Glow behavior
* Toxicity & scent

The result: emergent visual diversity from structured genetic rules.

---

### ✨ Fertility & Glow Signaling

Plants glow when fertile.
Glow behavior is genetically encoded:

* Maximum glow
* Pulse rate
* Ramp speed

Glow regions can be genetically linked to pattern and toxicity traits, forming inheritable “aura lineages.”

---

### 🧪 Mutation System

Genes mutate according to:

* Type-aware mutation logic (float, int, enum, bool, hue)
* Per-genome mutation rate
* Clamped trait ranges
* Rare structural variants

Mutation is controlled chaos, not noise.

---

### 🌎 Autonomous Ecosystem

* Wandering behavior
* Social attraction bias
* Species compatibility thresholds
* Fertility windows
* Lifespans
* Reproductive cooldowns

Plants self-organize into clusters and divergent species over time.

---

## 🎮 How To Run

Download the latest build from the **Releases** page.

Unzip and run:

```
FlowerGarden.exe
```

Click a plant to inspect its genetic summary.

Let it run.
Watch patterns emerge.
See what survives.

---

## 🔬 Why This Exists

This project explores:

* Procedural art
* Genetic systems modeling
* Emergent visual evolution
* Structural inheritance vs. pure randomness
* “Cozy chaos” as a design philosophy

It’s an ongoing sandbox for experimenting with:

* Linked trait evolution
* Structural genetic variation
* Visual lineage memory
* Small-scale artificial life systems

---

## 🛠 Built With

* Godot 4
* GDScript
* Gradient-based rendering
* Custom recombination logic

---

## 🧠 Future Directions

* Environmental pressures
* Predation & toxicity consequences
* Resource competition
* True diploid pairing & gamete modeling
* Persistent world seeds
* Save/load ecosystems
* Cross-species hybrid collapse

---

## 📜 License

MIT 

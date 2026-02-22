ZTH Core — Reference Implementation
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18726461.svg)](https://doi.org/10.5281/zenodo.18726461)

ZTH Core is the reference implementation of the structural engine introduced in the ZTH preprint series (Parts I–V).

Overview

The ZTH (Zone of Tension Harmonic) model represents the internal state of a system through six structural tensions:
	•	Resource (R)
	•	Quality (Q)
	•	Impact (I)
	•	Cost (C)
	•	Delay (D)
	•	Risk (Ri)

These tensions are mapped to a scalar structural value V using a promoter–inhibitor multiplicative formulation.

ZTH Core provides:
	•	The structural value function V
	•	The intrinsic structural reference Vi = 1
	•	The bounded human-scale mapping V_human in [0,2]
	•	A Shiny interface for structural exploration

This implementation serves as the computational backbone of the ZTH framework and ensures reproducibility of the structural model described in the associated publications.

⸻

Mathematical Structure

Structural value

V = (F+(R) × F+(Q) × F+(I)) / (F-(C) × F-(D) × F-(Ri))

Human-scale mapping

V_human = 1 + log(V) / log(V_max)

with symmetric logarithmic scaling around the intrinsic reference Vi = 1, mapping multiplicative structural deviations to a bounded interval V_human in [0,2].

⸻

Relation to the ZTH Research Series

ZTH Core accompanies the following preprints:
	•	Part I — Structural Model
	•	Part II — Intrinsic Structural Value
	•	Part III — Temporal Dynamics
	•	Part IV — Perceived Structural Value
	•	Part V — Cross-Domain Mapping

(See DOI references in the respective publications.)

⸻

Installation

Requirements
	•	R (>= 4.2)
	•	shiny
	•	fmsb

Run locally

shiny::runApp()

Citation

- ZTH Core (all versions): https://doi.org/10.5281/zenodo.18726461  
- ZTH Core v1.0.1: https://doi.org/10.5281/zenodo.18726462

License

MIT License.
# L_Systems
Generating artwork using random generation of L-Systems rules

## General
L-System is a formal language that was invented by Aristid Lindenmayer to model the growth and evolution of plants. It was also found useful in generating geometries such as fractals, space-filling curves and tiles. In this project I explore artworks that results from generating random L-Systems rules

The code was implemented in R

## examples

**Mondrian-like**

<img src="/examples/LS11_random2_6998.png" width="400" height="400" />

**Wires**

<img src="/examples/LS16_random8_97.png" width="400" height="400" />


## Scripts
Script `generate_LS.R` implements the function that receives L-Systems axion, rules and other parameters, generates the action string and then generates the object itself. It then translates the object to the axes origin

Following are the action utils I implemented (P, D, are production / drawing engine commands respectively):

* **F, L, R** - (P) replace with relevant rule. (D) draw forward at a given length and direction. F is used usually. L, R are used when different rules apply to the drawing command
* **f** - (P) replace with relevant rule. (D) go forward at a given length and direction but do not draw
* **+** - (D) add the predefined angle (turn left)
* **-** - (D) subtract the predefined angle (turn right)
* **[** - (D) push state for branching
* **]** - (D) pop state for end of branching
* **|** - (D) draw forward, no action replacement rule
* **X** - (D) mark a placeholder for external function
* **Any other letter** - (P) replace with relevant rule or ignore if no rule exists. (D) ignore

Script `new_random_LS` creates batches of L-System images. It then writes the images to a specified folder and the object parameters to a specified log file. Note that currently only one rule with F as re-writing symbol is supported

Script `generate_random_LS.R` Generates a L-System objects from its parameters in the log file. This is done by first generating the random rule and then calling the generate_LS function


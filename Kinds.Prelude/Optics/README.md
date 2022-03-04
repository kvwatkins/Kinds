<div id="top"></div>
<h3 align="center">Optics</h3>

<div>
  <p align="center">
   Composable Functional Structures 
  </p>
</div>

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
    </li>
    <li>
      <a href="#optic-types">Optic Types</a></a>
      <ul>
            <li><a href="#Lens">Lens</a></li>
            <li><a href="#Prism">Prism</a></li>
      </ul>
    </li>
  </ol>
</details>

## About The Project

This project introduces the notion of optics which are an every evolving field in functional programming currently [Profunctor optics, a categorical update]("https://arxiv.org/pdf/2001.07488.pdf") is the most current holistic view of optics
that seems to be available. To the uninitiated optics are cryptic at best and down right ugly at worst. They are however incredibly useful for advanced composition.

<p align="right">(<a href="#top">back to top</a>)</p>

## Modules

### Optic Types

Informally an optic in F# can be though of as a Context type that takes 4 generic parameters Optic<'s,'t,'a,'b> representing Structure('s,'t) and a focus('a,'b) in relation to each other in a before('s,'a) and after('t,'b) and two actions
which represent how the structure interacts with the focus.

The generic parameters represent

* 's = Structure | Before
* 'a = Focus | Before
* 't = Structure | After
* 'b = Focus | After

An optic in the form of O<'s,'s,'a,'a> is type preserving in its return type while O<'s,'t,'a,'b> is Polymorphic and changes the return type

### Lens

Functional Getters and accessors.

```f#
val create: get: ('s -> 'a) -> set: ('s -> 'b -> 't) -> Lens<'s,'t,'a,'b>
val view: l: Lens<'s,'t,'a,'b> -> s: 's -> 'a
val set: l: Lens<'s,'t,'a,'b> -> s: 's -> b: 'b -> 't
val over: l: Lens<'s,'t,'a,'b> -> s: 's -> f: ('a -> 'b) -> 't

type Person = { name : string }
let lens' = {get = (fun s -> s.name); set = (fun s a -> { s with name = a } )}

let kenny = {name = "kenny"} 

let result1 = view lens' kenny
val result1: string = "kenny"

let result2 = set lens' kenny "watkins"
val result2: Person = { name = "watkins" }

let result3  = over lens' kenny (fun f -> f + "'s name")
val result3: Person = { name = "kenny's name" }
 ```

<p align="right">(<a href="#top">back to top</a>)</p>

### Prism

Functional Constructors

```f#
val preview: p: Prism'<'s,'a> -> s: 's -> 'a option
val review: p: Prism'<'s,'a> -> a: 'a -> 's

type Person = { name : string }

let prism' = { build = (fun s -> {name = s})
                match_ = (fun s -> if s.name = "kenny" then
                                        Right(s.name)
                                    else Left(s) )}
    
let kenny = {name = "kenny"} 
let result1 = preview prism' kenny
val result1: string option = Some "kenny"

let watkins = {name = "watkins"} 
let result2 = preview prism' watkins
val result2: string option = None

let result3 = review prism' ""
val result3: Person = { name = "" }
 ```

<p align="right">(<a href="#top">back to top</a>)</p>
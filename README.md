# Euterpea-Oxun 

[![Haskell Stack CI](https://github.com/smoge/Euterpea-Oxum/actions/workflows/haskell.yml/badge.svg?branch=experimental)](https://github.com/smoge/Euterpea-Oxum/actions/workflows/haskell.yml)

![Screenshot from 2024-08-31 04-57-16](https://github.com/user-attachments/assets/71c93551-8f05-449d-9010-a13a0b1516ad)



## Original Attribution
This project is a fork of [Euterpea](http://www.euterpea.com/), originally created by:
- Paul Hudak
- Eric Cheng
- Hai (Paul) Liu
- Donya Quick
- Dan Winograd-Cort

While the original Euterpea is no longer actively maintained, its foundational work is a significant reference in music and functional programming. 





**To Clarence Barlow and Larry Polansky, in memoriam.**

## Initial Ideas




Euterpea-Oxun aims to update and extend Euterpea's capabilities to meet the needs of contemporary computer music. It will offer soft real-time control, flexibility to work with modern music materials (microtones, etc.), and integration with modern audio systems (SuperCollider, TinySynth, etc.).

To implement microtonal support properly, breaking compatibility with the original codebase is necessary. However, we will minimize breaking changes to only what is essential.



### Planned Features in progress
- Microtonal support with flexible pitch representation
- Music notation export/import
- Interval algebra
- SuperCollider-inspired pattern system
- RTM (rhythm trees)
- Soft real-time capability
- Property-based testing for all functionalities
- Performance benchmarking, performance awareness
- Parallel processing
- SuperCollider integration
- TinySynth audio engine experimental integration
- JACK MIDI support for low-latency MIDI messaging


## Project Status
Currently in early development.

- [Discussions](https://github.com/smoge/Euterpea-plus/discussions)

### Initial Focus 
- [ ] Core microtonal framework (rewriting Pitch, Accidental, etc)
- [ ] JACK MIDI integration
- [ ] Property-based testing
- [ ] Performance benchmarking
- [ ] Initial SuperCollider integration through already mature Haskell clients (hsc3 and vivid)

### Next
- Pattern language implementation
- Extended interval algebra
- TinySynth audio engine

## Contributing
We welcome contributions in:
- Musical theory implementation
- Performance optimization
- Audio system and SC3 integration
- DSP NRT synthesis
- Pattern system
- RTM implementation/integration
- Testing and validation

## License
Original Euterpea license for derived code. GPL3 for new code.

## Community
- Discussion board: theory and implementation
- GitHub and Wiki: documentation, ideas, and future guides

---

Links:
- [Discussions](https://github.com/smoge/Euterpea-plus/discussions)


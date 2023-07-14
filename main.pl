%apareceEn( Personaje, Episodio, Lado de la luz).
apareceEn( luke, elImperioContrataca, luminoso).
apareceEn( luke, unaNuevaEsperanza, luminoso).
apareceEn( vader, unaNuevaEsperanza, oscuro).
apareceEn( vader, laVenganzaDeLosSith, luminoso).
apareceEn( c3po, laAmenazaFantasma, luminoso).
apareceEn( c3po, unaNuevaEsperanza, luminoso).
apareceEn( c3po, elImperioContrataca, luminoso).
apareceEn( chewbacca, elImperioContrataca, luminoso).
apareceEn( yoda, elAtaqueDeLosClones, luminoso).
apareceEn( yoda, elAtaqueDeLosClones, luminoso).

%Maestro(Personaje)
maestro(luke).
maestro(leia).
maestro(vader).
maestro(yoda).
maestro(rey).
maestro(duku).

%caracterizacion(Personaje,Aspecto).
%aspectos:
% ser(Especie,Tamaño)
% humano
% robot(Forma)
caracterizacion(chewbacca,ser(wookiee,10)).
caracterizacion(luke,humano).
caracterizacion(vader,humano).
caracterizacion(yoda,ser(desconocido,5)).
caracterizacion(jabba,ser(hutt,20)).
caracterizacion(c3po,robot(humanoide)).
caracterizacion(bb8,robot(esfera)).
caracterizacion(r2d2,robot(secarropas)).

%elementosPresentes(Episodio, Dispositivos)
elementosPresentes(laAmenazaFantasma, [sableLaser]).
elementosPresentes(elAtaqueDeLosClones, [sableLaser, clon]).
elementosPresentes(laVenganzaDeLosSith, [sableLaser, mascara, estrellaMuerte]).
elementosPresentes(unaNuevaEsperanza, [estrellaMuerte, sableLaser, halconMilenario]).
elementosPresentes(elImperioContrataca, [mapaEstelar, estrellaMuerte] ).


%precede(EpisodioAnterior,EpisodioSiguiente)
precedeA(laAmenazaFantasma,elAtaqueDeLosClones).
precedeA(elAtaqueDeLosClones,laVenganzaDeLosSith).
precedeA(laVenganzaDeLosSith,unaNuevaEsperanza).
precedeA(unaNuevaEsperanza,elImperioContrataca).


%Desarrollo:

nuevoEpisodio(Heroe, Villano, Extra, Dispositivo) :-
    apareceEn(Heroe,_,_),
    apareceEn(Villano,_,_),
    apareceEn(Extra,_,_),
    personajesDiferentes(Heroe,Villano,Extra),
    jediPerpetuo(Heroe),
    villanoAmbiguo(Villano),
    condicionExtra(Extra,Heroe),
    esReconocible(Dispositivo).

nuevoEpisodio(Heroe, Villano, Extra, Dispositivo) :-
    apareceEn(Heroe,_,_),
    apareceEn(Villano,_,_),
    apareceEn(Extra,_,_),
    personajesDiferentes(Heroe,Villano,Extra),
    jediPerpetuo(Heroe),
    villanoAmbiguo(Villano),
    condicionExtra(Extra,Villano),
    esReconocible(Dispositivo).
    

personajesDiferentes(Personaje1,Personaje2,Personaje3):-
    Personaje1\=Personaje2,
    Personaje1\=Personaje3,
    Personaje2\=Personaje3.

jediPerpetuo(Jedi) :-
    maestro(Jedi),
    apareceEn(Jedi,_,luminoso),
    not((apareceEn(Jedi,_,Otro),Otro \= luminoso)).


villanoAmbiguo(Villano) :-
    apareceEn(Villano,Episodio1,_),
    apareceEn(Villano,Episodio2,_),
    Episodio1 \= Episodio2,
    enLadoOscuro(Villano).

enLadoOscuro(Villano) :-
    apareceEn(Villano,Episodio,luminoso),
    apareceEn(Villano,Episodio,oscuro).

enLadoOscuro(Villano) :-
    apareceEn(Villano,Episodio1,luminoso),
    apareceEn(Villano,Episodio2,oscuro),
    episodioAnteriorA(Episodio1,Episodio2).

episodioAnteriorA(Episodio1,Episodio2) :- precedeA(Episodio1,Episodio2).
episodioAnteriorA(Episodio1,Episodio2) :-
    precedeA(Episodio1,X),
    episodioAnteriorA(X,Episodio2).


condicionExtra(Extra,Personaje) :-
    esFiel(Extra,Personaje),
    esExotico(Extra).

esFiel(Extra,Personaje) :-
    apareceEn(Extra,_,_),
    forall(apareceEn(Extra,Episodiox,_),apareceEn(Personaje,Episodiox,_)).


esExotico(Extra) :-
    caracterizacion(Extra,robot(Forma)),
    Forma \= esfera.
esExotico(Extra) :-
    caracterizacion(Extra,ser(_,Tamanio)),
    Tamanio > 15.
esExotico(Extra) :-
    caracterizacion(Extra,ser(desconocido,_)).

esReconocible(Dispositivo) :-
    existeDispositivo(Dispositivo,_),
    findall(Episodio,existeDispositivo(Dispositivo,Episodio),Lista),
    length(Lista,Aux),
    Aux >= 3.

existeDispositivo(Dispositivo,Episodio):-
    elementosPresentes(Episodio,Lista),
    member(Dispositivo,Lista).
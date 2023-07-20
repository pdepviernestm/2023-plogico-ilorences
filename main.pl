%apareceEn( Personaje, Episodio, Lado de la luz).
apareceEn( luke, elImperioContrataca, luminoso).
apareceEn( luke, unaNuevaEsperanza, luminoso).
apareceEn( vader, unaNuevaEsperanza, oscuro).
apareceEn( vader, laVenganzaDeLosSith, luminoso).
apareceEn( vader, laAmenazaFantasma, luminoso).
apareceEn( c3po, laAmenazaFantasma, luminoso).
apareceEn( c3po, unaNuevaEsperanza, luminoso).
apareceEn( c3po, elImperioContrataca, luminoso).
apareceEn( chewbacca, elImperioContrataca, luminoso).
apareceEn( yoda, elAtaqueDeLosClones, luminoso).
apareceEn( yoda, laAmenazaFantasma, luminoso).

%Extras para consultas
apareceEn(leia,elImperioContrataca,luminoso).

apareceEn(r2d2,elImperioContrataca,luminoso).

apareceEn(yo,miEpisodio,oscuro).


%Punto3
apareceEn(jabba,elAtaqueDeLosClones,oscuro).

apareceEn(bebeYoda,unaNuevaEsperanza,luminoso).

apareceEn(robotin,laVenganzaDeLosSith,oscuro).


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

%Extras para consultas
caracterizacion(yo,ser(desconocido,1)).

%Punto3
caracterizacion(jabba,ser(babosa,100)).

caracterizacion(megaYoda,ser(verdoso,40)).

caracterizacion(robotin,robot(tactico)).



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


%Punto1

nuevoEpisodio(Heroe, Villano, Extra, Dispositivo) :-
    apareceEnAlguno(Heroe,Villano,Extra),
    personajesDiferentes(Heroe,Villano,Extra),
    jediPerpetuo(Heroe),
    villanoAmbiguo(Villano),
    esExotico(Extra),
    esFiel(Extra,Heroe,Villano),
    esReconocible(Dispositivo).
    

apareceEnAlguno(Heroe,Villano,Extra):-
    apareceEn(Heroe,_,_),
    apareceEn(Villano,_,_),
    apareceEn(Extra,_,_).

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

esFiel(Extra,Heroe,Villano) :-
    apareceEn(Extra,_,_),
    forall(apareceEn(Extra,Episodiox,_),apareceAlguno(Episodiox,Heroe,Villano)).


apareceAlguno(Episodio,Heroe,Villano) :- %La unica forma en la que me funcionaba (se que no es la mejor solucion)
    apareceEn(Heroe,Episodio,_),
    apareceEn(Villano,Episodio,_).
apareceAlguno(Episodio,Heroe,Villano) :-
    apareceEn(Heroe,Episodio,_),
    not(apareceEn(Villano,Episodio,_)).
apareceAlguno(Episodio,Heroe,Villano) :-
    not(apareceEn(Heroe,Episodio,_)),
    apareceEn(Villano,Episodio,_).

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

%Punto2
% 7 ?- nuevoEpisodio(Heroe,Villano,Extra,Dispositivo).
% Heroe = luke,
% Villano = vader,
% Extra = c3po,
% Dispositivo = sableLaser ;
% Heroe = luke,
% Villano = vader,
% Extra = c3po,
% Dispositivo = estrellaMuerte ;
%Con la base de conocimiento dada, se generan estas posibilidades (aunque se repiten indefinidamente)



%Consultas propias:
% 1 ?- nuevoEpisodio(luke,vader,c3po,estrellaMuerte).
% true ;
% 3 ?- nuevoEpisodio(leia,vader,r2d2,sableLaser).
% true 
% 4 ?- nuevoEpisodio(leia,vader,r2d2,clon).
% false.
% 6 ?- nuevoEpisodio(leia,vader,yo,sableLaser).
% false.

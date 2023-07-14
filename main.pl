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

% nuevoEpisodio(Heroe, Villano, Extra, Dispositivo) :-
%     apareceEn(Heroe,_,_),
%     apareceEn(Villano,_,_),
%     apareceEn(Extra,_,_),
%     personajesDiferentes(Heroe,Villano,Extra),
%     jediPerpetuo(Heroe).


personajesDiferentes(Personaje1,Personaje2,Personaje3):-
    Personaje1\=Personaje2,
    Personaje1\=Personaje3,
    Personaje2\=Personaje3.

jediPerpetuo(Jedi) :-
    maestro(Jedi),
    apareceEn(Jedi,_,luminoso),
    not((apareceEn(Jedi,_,Otro),Otro \= luminoso)).


% villanoAmbiguo(Villano) :-
%     apareceEn(Villano,Episodio1,_),
%     apareceEn(Villano,Episodio2,_),
%     Episodio1 \= Episodio2,
%     enLadoOscuro(Villano).

% enLadoOscuro(Villano) :-

% condicionExtra(Extra,Heroe,Villano) :-
%     esFiel(Extra,Heroe,Villano),
%     esExotico(Extra).

% esFiel(Extra,Heroe,Villano) :-
%     apareceEn(Extra,Episodio,_),
%     forall(apareceEn())

% esExotico(Extra) :-
%     caracterizacion(Extra,)


esReconocible(Dispositivo) :-
    elementosPresentes(_,Lista),
    findall(Dispositivo,(member(Dispositivo,Lista),elementosPresentes(_,Lista)),ListaAux),
    length(ListaAux,Aux),
    Aux >= 3.
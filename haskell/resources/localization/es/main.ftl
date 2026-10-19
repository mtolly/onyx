## numeric formatting (temporary, will be replaced with locale-based system)

# used in e.g. ogg vorbis quality
decimal-separator = ,
# used in e.g. score cutoffs
thousands-separator = .

## main console window

console = Terminal de Onyx

homepage = Página principal

version-checking = Buscando actualizaciones…
version-new = ¡Nueva versión disponible!
version-latest = Estas en la versión mas actual.
version-prerelease = Versión beta

function-load = Cargar una canción
function-batch = Recompilar canciones
function-quick = Convertidor/empaquetador rápido
function-other = Otras herramientas

welcome-header =
  Herramientas <color=magenta>Onyx</color> para juegos de ritmo, versión {$version}
  Elige una opción de abajo para empezar.

welcome-load =
  cambiar una canción de un juego a otro
  cambiar velocidad, reasignar instrumentos
  herramienta para previsualizar charts
welcome-batch =
  "Cargar una canción" pero para varias canciones
welcome-quick =
  editar canciones sin cambiarlas de juego
  crear paquetes o extraer paquetes
  cambiar formato de consola, e.g. <color=green>360</color> -> <color=cyan>PS3</color>
welcome-other =
  herramientas de MIDI/sonido
  herramientas de lipsync/dryvox

language-restart = Reinicia Onyx para cargar otro idioma.

## menu bar

menu-file = &Archivo
menu-file-load = {function-load}
menu-file-batch = {function-batch}
menu-file-quick = {function-quick}
menu-file-other = {function-other}
menu-file-close = Cerrar pantalla

menu-edit = &Edición
menu-edit-preferences = Preferencias

menu-help = &Ayuda
menu-help-readme = 'Readme'
menu-help-updates = Historial de actualizaciones
menu-help-license = Licencia
menu-help-credits = Créditos

menu-view = &Ver
menu-view-console = Mostrar terminal

## windows and tabs

window-quick-convert = Conversión rápida
window-tools = Herramientas
window-preferences = Preferencias de Onyx

tab-metadata = Metadatos
tab-instruments = Instrumentos
tab-preview = Previsualizar
tab-stars = Estrellas
tab-utilities = Utilidades
tab-task = Tareas
song-tab-ch-ps = CH/PS

pref-tab-general = General
pref-tab-rock-band = Rock Band
pref-tab-guitar-hero = Guitar Hero
pref-tab-clone-hero = Clone Hero
pref-tab-360 = 360
pref-tab-ps3 = PS3
pref-tab-wii = Wii
pref-tab-3d-preview = Vista previa de 3D
pref-tab-audio = Sonido

## shared gui widgets

load-files-counter =
  {$count ->
    [one] Se cargó {$count} archivo.
   *[other] Se cargaron {$count} archivos.
  }

task-show-files = Mostrar archivos creados
task-cancelled = Tareas canceladas.
task-status = {$done}/{$total} tareas completas: {$succeeded} exitosas, {$failed} con errores
task-number = {$number} tareas
task-created-files = Archivos creados:

instrument-guitar = Guitarra
instrument-bass = Bajo
instrument-drums = Batería
instrument-keys = Teclado
instrument-vocals = Vocales
instrument-other = Otro

speed-percent = Velocidad (%)
speed-percent-tooltip =
  Ajusta la velocidad del chart y su audio (sin cambiar el tono).

bass-pedal-1x = 1x pedal de bombo
bass-pedal-2x = 2x pedal de bombo
bass-pedal-both-output = Ambos
bass-pedal-both-input = 1x+2x pedal de bombo (formatos PS X+ o C3)

template = Plantilla

task-cancel = Cancelar

custom-title-suffix = Personalizar sufijo en el título
specific-song-id = Especifica el ID numérico de canción
part-none = (ninguna)

gh2-deluxe-features = Funciones de GH2 Deluxe
gh2-deluxe-1x-drums = 1x pedal de bombo
gh2-deluxe-2x-drums = 2x pedal de bombo

song-list-title = Elegir canciones
song-list-select-all = Elegir todas
song-list-select-none = Elegir ninguna
song-list-close = Cerrar
song-list-open =
  {$count ->
    [one] Abrir {$count} canción
   *[other] Abrir {$count} canciones
  }

warning-gh2-packs =
  ¡Asegúrate de combinar canciones en paquetes (dentro de 'Otras herramientas') antes de jugar! Cargar más de 16 paquetes corromperá tu partida de GH2.
warning-gh2-packs-long =
  ¡Advertencia! Cuando estas importando canciones a Guitar Hero II en Xbox 360, *necesitas* combinarlas en paquetes (dentro de 'Otras herramientas').
  ¡Cargar mas de 16 paquetes resultará en canciones erróneas y la corrupción de tu partida!
warning-gh3-cache =
  ¡Asegúrate de crear una caché de canciones para GH3 (dentro de 'Otras herramientas') de todas tus customs y DLC! Esto se requiere para cargar varias canciones.
warning-ghwor-cache =
  ¡Asegúrate de crear una caché de para GH:WoR (dentro de 'Otras herramientas') de todas tus customs y DLC! Esto se requiere para cargar varias canciones.
warning-ghwor-long =
  Por favor usa la pestaña de "Caché para WoR" dentro de 'Otras herramientas' después de convertir
  para producir el archivo que se necesita para cargar tus canciones.

  IMPORTANTE: Pueden ocurrir problemas cargando tantas canciones
  a Guitar Hero: Warriors of Rock, lo cual puede corromper tu partida.
  Por favor, ¡crea un respaldo de tu partida antes de cargar canciones customs!
warning-rr-renumber =
  Todas las canciones customs generadas para Rock Revolution tienen el mismo ID. Para cargar varias canciones, usa "Convertidor rápido > Renombrar RR" para reasignar los IDs.
warning-rb-quick-convert =
  Parece que tratas de convertir {
    $count ->
      [one] una canción para Rock Band
     *[other] canciones para Rock Band
  } de vuelta a RB3 sin modificaciones.
  Si no quieres cambiar de {
    $count ->
      [one] esta canción
     *[other] estas canciones
  } excepto la plataforma,
  deberías usar ‘Convertidor rápido’ para preservar {
    $count ->
      [one] el ID de la canción
     *[other] los IDs de canciones
  } para compatibilidad en línea.
warning-confirm = OK

game-short-clone-hero = CH
game-short-gh1 = GH1
game-short-gh2 = GH2
game-short-gh3 = GH3
game-short-ghwor = GH:WoR
game-short-rb1 = RB1
game-short-rb2 = RB2
game-short-rb3 = RB3
game-short-rock-revolution = RR

game-clone-hero = Clone Hero
game-gh1 = Guitar Hero 1
game-gh2 = Guitar Hero 2
game-gh3 = Guitar Hero III
game-ghwor = Guitar Hero: Warriors of Rock
game-rb1 = Rock Band 1
game-rb2 = Rock Band 2
game-rb3 = Rock Band 3
game-rock-revolution = Rock Revolution

game-tab-clone-hero = {game-clone-hero}
game-tab-gh1 = {game-short-gh1}
game-tab-gh2 = {game-short-gh2}
game-tab-gh3 = {game-short-gh3}
game-tab-rb2 = {game-short-rb2}
game-tab-rb3 = {game-short-rb3}
game-tab-ghwor = {game-short-ghwor}
game-tab-rock-revolution = {game-short-rock-revolution}

label-title = Título 
label-artist = Artista
label-album = Álbum
label-year = Año
label-author = Autor
label-genre = Género
label-subgenre = Subgénero

file-picker-create-diy-folder = Crear carpeta manual
file-picker-create-folder = Crear carpeta
file-picker-load-album-art = Elegir portada
file-picker-load-dryvox-source-audio = Cargar canción o audio de vocales
file-picker-load-hdr = Elegir archivo .HDR
file-picker-load-midi = Elegir archivo MIDI
file-picker-load-milo = Elegir archivo .milo
file-picker-load-song = Cargar canción
file-picker-save-ch-folder = Guardar carpeta de canción de CH/PS
file-picker-save-ch-sng = Guardar archivo .sng de CH/PS
file-picker-save-ch-zip = Guardar archivo .zip de CH/PS
file-picker-save-gh2-live = Guardar archivo LIVE de GH2
file-picker-save-gh3-cache-360 = Guardar archivo de caché para GH3 (360)
file-picker-save-gh3-cache-ps3 = Guardar archivo de caché para GH3 (PS3)
file-picker-save-gh3-live = Guardar archivo LIVE de GH3
file-picker-save-gh3-pkg = Guardar archivo PKG de GH2
file-picker-save-ghwor-live = Guardar archivo LIVE de GH:WoR
file-picker-save-ghwor-pkg = Guardar archivo PKG de GH:WoR
file-picker-save-lipsync-audio = Guardar audio de 'lipsync'
file-picker-save-magma-v2 = Guardar proyecto de MAGMA v2
file-picker-save-midi = Guardar archivo MIDI
file-picker-save-milo = Guardar .milo
file-picker-save-multitrack-audio = Guardar archivo .{$extension}
file-picker-save-rb2-con = Guardar archivo CON de RB2
file-picker-save-rb2-pkg = Guardar archivo PKG de RB2
file-picker-save-rb3-con = Guardar archivo CON de RB3
file-picker-save-rb3-pkg = Guardar archivo PKG de RB3
file-picker-save-reaper = Guardar proyecto de REAPER
file-picker-save-reduced-midi = Guardar MIDI reducido
file-picker-save-rr-live = Guardar archivo LIVE de RR
file-picker-save-rr-pkg = Guardar archivo PKG de RR
file-picker-save-voc = Guardar archivo .voc
file-picker-save-web-preview = Guardar previsualización de web
file-picker-save-wor-cache-360 = Guardar archivo de caché para WoR (360)
file-picker-save-wor-cache-ps3 = Guardar archivo de caché para WoR (PS3)
file-picker-select-folder = Elegir carpeta

file-type-package =
  .tree-root = Paquetes
  .add = Agregar paquete
  .load = Cargar paquete
  .clear = Borrar paquetes
file-type-rb-con-live =
  .tree-root = CON/LIVE de Rock Band
  .add = Agregar CON/LIVE de Rock Band
  .load = Cargar CON/LIVE de Rock Band
  .clear = Borrar CON/LIVE de Rock Band
file-type-con-live =
  .tree-root = CON/LIVE
  .add = Agregar CON/LIVE
  .load = Cargar CON/LIVE
  .clear = Borrar CON/LIVE
file-type-rb-song =
  .tree-root = Canciones de Rock Band
  .add = Agregar canción de Rock Band
  .load = Cargar canción de Rock Band
  .clear = Borrar canciones de Rock Band
file-type-rr-song =
  .tree-root = Canciones de RR
  .add = Agregar canción de RR
  .load = Cargar canción de RR
  .clear = Borrar canciones de RR
file-type-ch-song =
  .tree-root = Canciones de CH
  .add = Agregar canción de CH
  .load = Cargar canción de CH
  .clear = Borrar canciones de CH
file-type-audio =
  .tree-root = Audio
  .add = Agregar audio
  .load = Cargar audio
  .clear = Borrar audio
file-type-song =
  .tree-root = Canciones
  .add = Agregar canción
  .load = Cargar canción
  .clear = Borrar canciones

drums-mode-4lane = Batería de 4 líneas
drums-mode-5lane = Batería de 5 líneas
drums-mode-pro = Batería PRO
drums-mode-real = Batería 'Real' de Phase Shift
drums-mode-elite = Batería 'Elite'

drums-rb3-kit-hard-rock = Batería de Hard Rock
drums-rb3-kit-arena = Batería de estadio
drums-rb3-kit-vintage = Batería clásica
drums-rb3-kit-trashy = Batería cutre
drums-rb3-kit-electronic = Batería electrónica

mode-5fret = 5 trastes
mode-6fret = 6 trastes
mode-pro-keys = Teclado PRO
mode-pro-guitar = Guitarra PRO
mode-drums = Batería
mode-vocals = Vocales
mode-mania = Mania

gender-female = Femenino
gender-male = Masculino
gender-unspecified = Género no especificado

language-english = Ingles
language-french = Francés
language-italian = Italiano
language-spanish = Español
language-german = Alemán
language-japanese = Japones

vocals-solo = Solo
vocals-harmonies-2 = Armonías (2)
vocals-harmonies-3 = Armonías (3)

toggle-ps2-practice-audio = Generar sonido de modo práctica para PS2

template-runner-tooltip =
  Estructura de ubicación para archivos nuevos.
    %carp_de_entrad% - carpeta que contiene la entrada
    %input_base% - nombre de archivo de entrada sin extensión
    %modifiers% - características distintivas e.g. modificador de velocidad
    %title% - título desde los metadatos de la canción (incluyendo modificadores)
    %artist% - artista desde los metadatos de la canción
    %album% - álbum desde los metadatos de la canción
    %author% - autor desde los metadatos de la canción
    %song_id% - ID único de canción

terminal-task-start = <color=cyan>{$task-name}</color>: {$task-path}
terminal-task-warning = <color=yellow>Warning</color>: {$message}
terminal-task-error = <color=red>¡ERROR!</color> {$message}
terminal-task-success = <color=green>¡Éxito!</color> {$message}

## batch recompile

batch-tab-songs = Canciones
batch-tab-songs-import-label = Importar:

batch-tab-song-entry =
  {$has-title ->
    [false] Sin título
   *[true] {$title}
  }{$has-artist ->
    [false] {""}
   *[true] {" "}({$artist})
  }{$is-2x ->
    [true] {" (2x)"}
   *[false] {""}
  }
batch-tab-songs-data-author =
  Autor: {$author}
batch-tab-songs-data-format =
  Formato: {$format}
batch-tab-songs-data-path =
  Ubicación: {$path}{
    $index ->
      [none] {""}
     *[some] {" "}(#{$index})
  }

batch-tab-rb3-all-toms = Convertir todas las notas de batería sin pro a Toms
  .tooltip = Si activas esta opción, se agregaran marcadores de Tom a todas las notas al importar un chart de FoF/PS/CH donde no se detecte Batería Pro.
batch-tab-gh-drums-5lane = Batería Pro a 5 líneas
batch-tab-gh-drums-4lane = Batería Pro a 4 líneas
batch-tab-rr-drums-6pad = Batería Pro a 6 pads
batch-tab-rr-drums-4pad = Batería Pro a 4 pads

batch-configuration-default = Configuración de partes por defecto
batch-configuration-rb3-fill-guitar-drums =
  Rellenar partes vacías primero con guitarra y luego con batería
batch-configuration-rb3-fill-guitar =
  Copiar guitarra a bajo/teclado/batería
batch-configuration-rb3-fill-drums =
  Copiar batería/baile a guitarra/bajo/teclado
batch-configuration-rb2-guitar-to-bass-empty =
  Copiar guitarra a bajo si bajo esta vació
batch-configuration-rb2-guitar-to-bass =
  Copiar guitarra a bajo
batch-configuration-rb2-keys-to-guitar-bass-empty =
  Teclado en guitarra/bajo si están vacíos
batch-configuration-rb2-keys-to-guitar =
  Teclado en guitarra
batch-configuration-rb2-keys-to-bass =
  Teclado en bajo
batch-configuration-rb2-drums-to-guitar-bass-empty =
  Copiar batería/baile a guitarra/bajo si están vacíos
batch-configuration-rb2-drums-to-guitar-bass =
  Copiar batería/baile a guitarra/bajo
batch-configuration-ch-drums-to-guitar-empty =
  Copiar batería/baile a guitarra si guitarra esta vacía
batch-configuration-ch-drums-to-guitar =
  Copiar batería/baile a guitarra
batch-configuration-ch-drums-to-rhythm-empty =
  Copiar batería/baile a ritmo si ritmo esta vació
batch-configuration-ch-drums-to-rhythm =
  Copiar batería/baile a ritmo
batch-configuration-ch-guitar-drums-fill =
  Copiar guitarra a batería o viceversa si falta alguna

batch-task-360-con = Crear archivos CON de Xbox 360
batch-task-ps3-pkg = Crear archivos PKG de PS3
batch-task-rpcs3-pkg = Crear archivos PKG de RPCS3
batch-task-ps3-folder = Crear carpetas para PS3/RPCS3
batch-task-magma = Crear proyectos de Magma
batch-task-ch-folder = Crear carpetas de CH
batch-task-ch-sng = Crear archivos .sng de CH
batch-task-ch-zip = Crear zips de CH
batch-task-ps2-diy = Crear carpeta manual de PS2
batch-task-ps2-add = Agregar al ARK de canciones Bonus para PS2
batch-task-360-live = Crear archivos LIVE para Xbox 360
batch-task-preview = Crear vistas previas de web

## single song view

song-toggle-2x-bass-pedal = Batería de 2x pedal de bombo

part-guitar = Guitarra
part-bass = Bajo
part-keys = Teclado
part-drums = Batería
part-vocal = Vocales
part-rhythm = Ritmo
part-guitar-coop = Guitarra Coop
part-vocal-animation = Vocal (animaciones)

coop-bass = Coop: Bajo
coop-rhythm = Coop: Ritmo

numberbox-gh5-song-id = Custom Song ID (dlc)
numberbox-gh5-cdl = Custom Package ID (cdl)
numberbox-rr-song-id = Custom Song ID
numberbox-rr-leaderboard-id = Custom Leaderboard ID

create-360-con = Crear archivo CON de Xbox 360
create-ps3-pkg = Crear archivo PKG de PS3
create-magma = Crear proyecto de Magma
create-ps3-folder = Crear carpeta para PS3/RPCS3
create-360-live = Crear archivo LIVE de Xbox 360
create-rpcs3-pkg = Crear archivo PKG de RPCS3
create-ch-folder = Crear carpeta de canción para CH/PS
create-ch-sng = Crear archivo .song de CH
create-ch-zip = Crear zip de CH/PS
create-ps2-add-bonus = Agregar al ARK de canciones Bonus para PS2
create-ps2-diy = Crear carpeta manual de PS2

stars-rb3-title = Umbral de estrellas para Rock Band 3
stars-gh-title = Umbral de estrellas para Guitar Hero 1/2
stars-lead = Principal
stars-coop = Coop
stars-1 = 1*: {$score}
stars-2 = 2*: {$score}
stars-3 = 3*: {$score}
stars-4 = 4*: {$score}
stars-5 = 5*: {$score}
stars-gold = G*: {$score}
stars-gh-base = Base: {$score}
stars-gh-4 = 4*: {$score}
stars-gh2-5 = 5* (GH2): {$score}
stars-gh1-5 = 5* (GH1): {$score}

diff-easy = (F)
diff-medium = (M)
diff-hard = (D)
diff-expert = (X)

meta-convert = Convertir
meta-rhythm-keys = Teclado rítmico
meta-rhythm-bass = Bajo rítmico
meta-auto-emh = Reducciones automáticas
meta-expert-only = Solo experto
meta-cover = 'Cover'
label-track-number = #
default-song-title = Canción

tier = Nivel {$number}
rank = Grado

audio-plan-select = Elegir plan de audio
loading-image = Cargando imagen: {$path}

preview-view-tracks = Ver pistas
preview-audio = Sonido
preview-background = Fondo
preview-bg-none = Ninguno

build-rb3-con = Construyendo archivo CON para RB3 (360)
build-rb3-pkg = Construyendo archivo PKG para RB3 (PS3)
build-magma = Construyendo proyecto de Magma
build-rb3-ps3 = Creando carpetas de RB3 para PS3
build-rb2-con = Construyendo archivo CON para RB2 (360)
build-rb2-pkg = Construyendo archivo PKG para RB2 (PS3)
build-rb2-ps3 = Creando carpetas de RB2 para PS3
build-ch-folder = Creando carpetas para CH/PS
build-ch-zip = Construyendo archivo zip para CH/PS
build-ch-sng = Construyendo archivo .sng para CH/PS
build-gh1-ark = Agregando al archivo ARK de GH1
build-gh1-diy = Creando carpeta manual para GH1 (PS2)
build-gh2-live = Construyendo archivo LIVE para GH2
build-gh2-ark = Agregando al archivo ARK de GH2
build-gh2-diy = Creando carpeta manual para GH2 (PS2)
build-gh3-live = Construyendo archivo LIVE para GH3
build-gh3-pkg = Construyendo archivo PKG para GH3
build-gh3-ps3 = Creando carpetas de GH3 para PS3
build-ghwor-live = Construyendo archivo LIVE para GH:WoR
build-ghwor-pkg = Construyendo archivo PKG para GH:WoR
build-ghwor-ps3 = Creando carpetas de GH:WoR para PS3
build-rr-live = Construyendo archivo LIVE para RR
build-rr-pkg = Construyendo archivo PKG para RR

## quick convert

quick-convert-tab-rb = Conversión rápida + creador de paquetes RB
quick-convert-tab-ch = Conversión rápida CH
quick-convert-tab-rr-renumber = Reorganizar RR
quick-convert-tab-gh2-pack = Creador de paquetes GH2
quick-convert-tab-rb-legacy-pkg = CON->PKG de legado RB

quick-convert-limit-easy-medium-frets = Limitar trastes de Fácil/Medio
  .tooltip = Asegura que fácil/medio usen 3/4 trastes respectivamente en cinco líneas.

quick-convert-rb-black-venue = VENUE negra
  .tooltip = Reemplaza la pista de VENUE con una que intenta imitar un fondo negro. Funciona en canciones previas a RB3 o canciones en formato RBN2; no funciona con canciones de RB3 en formato .milo.
quick-convert-rb-no-overdrive = Sin sobrecargar
  .tooltip = Elimina todas las frases de sobrecargar en todos los instrumentos.
quick-convert-rb-no-lanes-gbk = Sin carriles (G/B/T)
  .tooltip = Elimina carriles de tremolo y trino de guitarra/bajo/teclado y sus versiones de Pro. No elimina glissandos de Teclado Pro.
quick-convert-rb-no-lanes-drums = Sin carriles (batería)
  .tooltip = Elimina carriles de redobles de uno o dos parches de Batería.
quick-convert-rb-no-drum-fills = Sin rellenos en batería
  .tooltip = Elimina los rellenos de activación de batería. No elimina los finales apoteósicos. Esta función es principalmente para grabar vídeos.
quick-convert-rb-force-22-protar = Forzar Protarra de 22 trastes
  .tooltip = En canciones con Guitarra/Bajo que tienen versiones distintas para Mustang y Squier, se elimina la versión de Mustang, lo cual deja solo la versión de Squier. Esta función es principalmente para grabar vídeos.
quick-convert-rb-unmute-22-protar = Desmut. traste >22 en Protarra
  .tooltip = Cualquier nota sobre traste 22 en Guitarra/Bajo PRO no estará silenciada. Esto permite que la chart pase por MAGMA y luego se desmutean al pasar por Onyx para grabar vídeos. Toma en cuenta que estas notas no pueden ser tocadas de maneras legitimas.
quick-convert-rb-decompress-milo = Descomprimir .milo_*
  .tooltip = Descomprime cualquier archivo .milo_xbox, etc. Es probable que ForgeTool requiera esto para leer archivos durante el proceso de conversión a RB4.
quick-convert-rb-author-to-tag = Colocar Autor en DTA
  .tooltip = Copia la información de autor en formato C3 al DTA para que se pueda usar en proyectos como RB2/RB3 Deluxe.
quick-convert-rb-strip-title-tags = Eliminar etiquetas de título
  .tooltip = Elimina las siguiente etiquetas de los titulos: (2x Bass Pedal) y sellos de autor (Z) (X) (O) etc.
quick-convert-rb-note-shuffle = Aleatorizar notas
  .tooltip = Notas en partes de 5 trastes y batería serán aleatorizadas.
quick-convert-rb-decrypt-moggs = Desencriptar MOGGs
  .tooltip = Desencripta todos los archivos MOGG. Toma en cuenta que MOGGs desencriptados no funcionan en PS3 sin RB3Enhanced o Rock Band 3 Deluxe.

quick-convert-ch-chart-to-midi = .chart a MIDI
  .tooltip = Convierte todos los archivos .chart a .mid.
quick-convert-ch-ps-tap-open = 'Taps'/'opens' en formato PS
  .tooltip = Codifica notas 'tap' y 'open' usando el formato viejo de Phase Shift (mensajes SYSEX). Esto reducirá el tamaño de las notas sostenidas para eliminar notas superpuestas y notas 'open' en acordes.
quick-convert-ch-audio-ogg = Audio a .ogg
  .tooltip = Codifica archivos de audio al formato .OGG si es que no lo están ya.
quick-convert-ch-audio-opus = Audio a .opus
  .tooltip = Codifica archivos de audio al formato Opus si es que no lo están ya.
quick-convert-ch-real-drums-to-elite = Batería 'Real' a 'Elite'
  .tooltip = Si la canción tiene una parte de Batería 'Real', agrega una conversión a Batería 'Elite'.

quick-convert-mode-in-place = Reemplazar los archivos originales con versiones nuevas
quick-convert-mode-one-to-one = Cada archivo de entrada produce un archivo nuevo
quick-convert-mode-make-packs = Crear paquetes: Unir canciones respetando el tamaño máximo 
quick-convert-mode-make-songs = Crear canciones: Producir un archivo nuevo para cada canción
quick-convert-mode-make-dolphin = Crear paquete de Dolphin (Wii)

quick-convert-format-con = CON (360)
quick-convert-format-live = LIVE (360)
quick-convert-format-pkg = PKG (PS3)
quick-convert-format-folders = Carpetas (PS3)

quick-convert-encrypt-edat = Encriptar .mid.edat
quick-convert-for-ps3-files = Para archivos de PS3:

quick-convert-folder-keep = Preservar sub-carpetas USRDIR originales
quick-convert-folder-keep-any = Preservar sub-carpetas USRDIR originales (si existen)
quick-convert-folder-combine = Combinar a una sub-carpeta USRDIR nueva
quick-convert-folder-combine-per-file = Combinar a una sub-carpeta USRDIR nueva por cada archivo
quick-convert-folder-combine-per-pack = Combinar a una sub-carpeta USRDIR nueva por cada paquete
quick-convert-folder-each = Cada canción tiene una sub-carpeta USRDIR nueva
quick-convert-folder-custom-initial = Nombrar sub-carpeta USRDIR…
quick-convert-folder-custom = Carpeta personalizada: {$folder}
quick-convert-folder-custom-prompt = Ingresa el nombre de la sub-carpeta de USRDIR para tu paquete. (Se agregará un número si se crea mas de un paquete.)

quick-convert-max-pack-size = Tamaño max. de paq. (MiB)
quick-convert-invalid-pack-size = Tamaño inválido
quick-convert-no-songs-added = No hay canciones
quick-convert-make-packs-count =
  {$count ->
    [one] Crear {$count} paquete
   *[other] Crear {$count} paquetes
  }
quick-convert-unsupported-format = Formato no permitido: 'transformar en lugar' solo puede convertir CON/LIVE/PKG
quick-convert-empty = (vacio)
quick-convert-songs-incl = {$count} canciones inclu. {$title}
quick-convert-save-pack-file = Guardar archivo de paquete
quick-convert-save-pack-template = Elegir plantilla para paquetes (se agregará un numero a cada paquete)
quick-convert-pack-number = Paquete #{$number}

quick-convert-fof-folder = formato carpeta
quick-convert-fof-sng = formato .sng
quick-convert-fof-encore = formato Encore
quick-convert-mode-to-folders = Cada canción se guardará a su propia carpeta
quick-convert-mode-to-sng = Cada canción se guardará a un archivo .sng

quick-convert-ch-warning-base = Esta opción modificará la canciones elegidas y las eliminará las versiones viejas.
quick-convert-ch-warning-encore =
  WARNING: Esto formato convertirá canciones a formato CH y eliminará los archivos originales.
  Elige otro modo para preservarlos.
quick-convert-ch-ops-header = Según la plantilla de nombres, se cumplirán estas operaciones:
quick-convert-ch-op-overwrite =
  {$songs ->
    [one] - {$songs} canción será sobrescrita con una versión nueva
   *[other] - {$songs} canciones serán sobrescritas con versiones nuevas
  }
quick-convert-ch-op-convert =
  {$songs ->
    [one] - {$songs} canción será convertida a una versión nueva
   *[other] - {$songs} canciones serán convertidas a versiones nuevas
  }
quick-convert-ch-op-convert-delete =
  {$songs ->
    [one] - {$songs} canción será convertida a una versión nueva y se eliminará la versión vieja
   *[other] - {$songs} canciones serán convertidas a versiones nuevas y se eliminarán la versiones viejas
  }
quick-convert-ch-ops-encore = ADVERTENCIA: Ciertas canciones en formato Encore serán eliminadas/reemplazadas con versiones compatibles con CH

quick-convert-delete-originals = Eliminar archivos originales
quick-convert-dolphin-preview-audio = Intenta generar audio de previsualización
quick-convert-task-make-dolphin = Crear paquete de Dolphin

task-legacy-con-to-pkg-rb2 = Convertir a PKG (RB2)
task-legacy-con-to-pkg-rb3 = Convertir a PKG (RB3)

pack-game-from-first = Juego (empezando por el primero):
pack-game = Juego:
pack-save-stfs = Guardar archivo STFS
pack-name = Nombre de paquete
pack-description = Descripción de paquete
pack-make-con = Crear paquete CON (RB3/RB2)
pack-make-live = Crear paquete LIVE (el resto de los juegos)
pack-make-extracted = Crear carpeta extraída
pack-save-as = Guardar paquete como…
pack-task-extracted = Guardar contenido extraído del paquete: {$path}
pack-task-con = Guardar paquete como CON: {$path}
pack-task-live = Guardar paquete como LIVE: {$path}

button-select-folder = Elegir carpeta…

song-count =
  {$count ->
    [one] {$count} canción
   *[other] {$count} canciones
  }

## misc tools

misc-tab-midi-stuff = Cosas de MIDI
misc-tab-mogg-vgs-fsb = MOGG/VGS/FSB
misc-tab-lipsync = 'Lipsync'
misc-tab-dry-vox = 'Dry vox'
misc-tab-milo = .milo
misc-tab-rb3-cache = Caché de RB3
misc-tab-gh3-cache = Caché de GH3
misc-tab-ghwor-cache = Caché de GH:WoR

misc-button-extract-milo = Extraer .milo a carpeta
misc-button-repack-milo = Empaquetar .milo de carpeta
misc-button-gh3-cache-360 = Crear caché de GH3 (360)
misc-button-gh3-cache-ps3 = Crear caché de GH3 (PS3)
misc-button-gh3-cache-install-rpcs3 = Instalar caché de GH3 (RPCS3)
misc-button-ghwor-cache-360 = Crear caché de GH:WoR (360)
misc-button-ghwor-cache-ps3 = Crear caché de GH:WoR (PS3)
misc-button-ghwor-cache-install-rpcs3 = Instalar caché de GH:WoR (RPCS3)

misc-task-make-gh3-cache = Crear archivo caché de GH3
misc-task-install-gh3-cache = Instalar archivo caché de GH3
misc-task-make-ghwor-cache = Crear archivo caché de GH:WoR
misc-task-install-ghwor-cache = Instalar archivo caché de GH:WoR

label-midi-or-chart = MIDI o .chart
midi-load-dialog = Cargar MIDI o .chart file
midi-button-reduce = Rellenar dificultades bajas y animaciones de batería
midi-task-reduce = Reducir MIDI: {$path}
midi-button-prokeys = Detectar notas colgadas
midi-task-prokeys = Revisar rango de Teclado Pro: {$path}
midi-button-reaper = Crear proyecto de REAPER con plantilla de RB
midi-task-reaper = Crear proyecto REAPER: {$path}
midi-button-chart-to-midi = Conversión limpia de .chart a MIDI
midi-task-chart-to-midi = Convertir a MIDI: {$path}
midi-button-batch = Procesar por lotes
midi-task-chart-batch = Convertir .chart a MIDI (por lotes): {$path}

label-con-folder = Carpeta CON
label-song-cache = Caché de canciones
cache-load-dialog = Cargar caché de canciones para RB3
cache-button-apply = Aplicar IDs numéricos desde caché de canciones
cache-task = Caché con IDs de codificación rígida: {$path}

util-button-preview = Crear previsualización de web
util-button-reduce = Producir MIDI con reducciones automáticas
util-save-midi-dialog = Guardar archivo MIDI
util-task-prokeys = Verificar rangos de Teclado Pro

mogg-button-ogg = Ogg a MOGG, sin re-codificar ({
  $channels ->
    [one] {$channels} canal
   *[other] {$channels} canales
  })
mogg-button-combine = Combinar a MOGG ({
  $channels ->
    [one] {$channels} canal
   *[other] {$channels} canales
  })
mogg-button-vgs = Combinar a VGS ({
  $channels ->
    [one] {$channels} canal
   *[other] {$channels} canales
  })
mogg-button-fsb3 = Crear FSB3-XMA1 ({
  $streams ->
    [one] {$streams} stream
   *[other] {$streams} streams
  }, {
  $channels ->
    [one] {$channels} canal
   *[other] {$channels} canales
  } total)
mogg-button-fsb4 = Crear FSB4-XMA2 ({
  $channels ->
    [one] {$channels} canal
   *[other] {$channels} canales
  })
mogg-task-ogg = Ogg a MOGG
mogg-task-mogg = Creación de archivo MOGG
mogg-task-vgs = Creación de archivo VGS
mogg-task-fsb3 = Creación de archivo FSB3
mogg-task-fsb4 = Creación de archivo FSB4
audio-format-channels =
  {$format}, {$channels ->
    [one] {$channels} canal
   *[other] {$channels} canales
  }
audio-duration =
  {$duration} ({$frames ->
    [one] {$frames} fotogramas
   *[other] {$frames} fotogramas
  } @ {$rate} Hz)

label-midi-file = Archivo MIDI
label-audio-file = Archivo de sonido
label-transition-ms = Transición (ms)

dryvox-blank = (vació)

misc-button-make-voc = Crear .voc de GH2 .voc de PART VOCALS
misc-button-make-milo = Crear .milo_* de RB3
  .tooltip = Crear un archivo .milo_* de RB3 de lo que este presente primero: pistas de LIPSYNC*, pistas de HARM* o PART VOCALS.
misc-button-update-milo = Actualizar .milo_* de RB3/TBRB
  .tooltip = Actualiza un archivo .milo_* de RB3 con pistas LIPSYNC*, pistas de HARM* o PART VOCALS. O actualiza archivo .milo_* de TBRB desde pistas LIPSYNC_[beatle].
misc-button-create-lipsync = Crear *.lipsync
  .tooltip = Exportar todas las pistas de LIPSYNC* y LIPSYNC_[beatle] a archivos .lipsync, listos para ser importados a archivos .milo_*.
misc-button-vocal-lipsync-rb3 = Convertir pistas de vocales a pistas LIPSYNC* para RB3
  .tooltip = Usar HARM* o PART VOCALS para producir pistas de LIPSYNC{"{"}1-3{"}"} que contienen información Viseme, usando el set de expresiones para RB.
misc-button-vocal-lipsync-tbrb = Convierte pistas de vocales a pistas de LIPSYNC* para TBRB
  .tooltip = Usa HARM* o PART VOCALS para producir pistas de LIPSYNC{"{"}1-3{"}"} que contienen información Viseme, usando el set de expresiones para TBRB.
misc-button-dryvox-sine = Crear dryvox de sinusoides de pistas de MIDI
misc-button-dryvox-clipped = Crear dryvox recortado de la pistas de MIDI y audio

misc-task-extract-milo = Extraer {$path}
misc-task-repack-milo = Re-empaquetar {$path}
misc-task-make-voc = Crear .voc de GH2: {$path}
misc-task-make-milo = Crear .milo con lipsync de MIDI: {$path}
misc-task-update-milo = Actualizar .milo con lipsync de MIDI: {$path}
misc-task-export-lipsync = Exportar .lipsync a carpeta
misc-task-vocal-to-lipsync = Convertir pistas de vocales a pistas de LIPSYNC: {$path}
misc-task-dryvox = {$name}: {$path}

## preferences

button-save = Guardar
button-start = Iniciar

pref-unlimited-cores = Hilos ilimitados
pref-core-limit = Limite de hilos
pref-detect-muted = Para juegos sin notas 'open', convertir rasgueos "muteados" al estilo de Harmonix
  .tooltip = Si activas esta opción, secuencias en forma de "acorde, 'open' rasgueado, acorde rasgueado" son asumidas que representan rasgueos silenciados. En vez de mover las notas a verde, las notas abiertas serán movidas a ser la nota mas baja del acorde, o el acorde anterior si esta mas cerca a las notas abiertas.
pref-limit-reduced-frets = Cuando se generan las dificultades bajas de pistas de 5 trastes, asegura que fácil/medio usen 3/4 trastes respectivamente

pref-magma-require = Magma requerido
pref-magma-try = Magma opcional
pref-magma-disable = No utilizar Magma
pref-black-venue = Siempre exportar VENUE negra
pref-label-2x = Etiquetar canciones de 2x pedal de bombo con (2x Bass Pedal) por defecto
pref-legal-tempos = Modificar tempos en exportación de RB para seguir las reglas de Magma
  .tooltip = Magma requiere que los archivos MIDI utilicen tempos en un rango de 40 a 300 BPM. Esto es un rango mas limitado que los juegos permiten.
pref-rb3-encoding-label = Recompilar .dta de RB3
pref-pack-encoding-label = Paquete .dta de RB3
encoding-latin1 = Latin-1
encoding-utf8 = UTF-8
encoding-as-is = Tal cual

pref-gh2-offset = Compensación de audio para GH2 (ms)
  .tooltip = Ajusta la compensación del sonido para exportaciones de Guitar Hero II, principalmente para uso con emulador. Valores positivos apresuran el sonido para compensar por el retraso. Valores negativos mueve el sonido para que suene mas tarde.
pref-sort-gh2 = Organizar canciones bonus de GH2 en el .ARK
pref-artist-sort = Para GH2 y GH3, organizar canciones bonus por artista antes que título
  .tooltip = Esto afecta las canciones bonus de GH2 en el .ARK (cuando activas esta opción) y DLC de GH3 (como se organiza en el archivo de caché).

pref-dir-ch = Carpeta por defecto de CH
pref-ch-downmix = Reducir multipistas para CH
pref-ch-audio-ogg = Exportar audio Ogg Vorbis
pref-ch-audio-opus = Exportar audio Opus

pref-dir-rb = Carpeta para CON/LIVE
pref-trim-xbox = Limitar nombre de archivos para USBs o discos duros de Xbox 360
  .tooltip = Si activas esta opción, los nombres de archivos CON o LIVE se limitaran 42 caracteres y eliminará caracteres de 'más' y comas (una restricción que existe para discos duros de 360, pero no USB).
pref-rb-number-id = Usar números validos en vez de símbolos para archivos de RB para Xbox 360
  .tooltip = Si activas esta opción, archivos de Rock Band usara un número entero al azar para generar song_id. De lo contrario, una 'o' será prefija para formar el símbolo y el juego generara un ID aleatoriamente. Un numero entero valido se requiere para jugar online en RGH, para conversión de PS3 y para mantener puntuaciones cuando se regenere la caché de canciones.

pref-ps3-encrypt = Encriptar archivos .mid.edat para PS3
  .tooltip = Si activas esta opción, los archivos .mid.edat para customs de PS3 serán encriptadas. Esto se requiere para jugadores de PS3 en vanilla pero opcional y menos conveniente para usuarios de Deluxe o RPCS3.
pref-dir-ps3 = Carpeta de PS3/RPCS3

pref-dir-wii = Carpeta de .app para Wii

pref-msaa-none = Sin MSAA
pref-msaa-2x = MSAA 2x
pref-msaa-4x = MSAA 4x
pref-msaa-8x = MSAA 8x
pref-msaa-16x = MSAA 16x
pref-fxaa = FXAA
pref-fps = FPS
pref-elite-hihat-default = Lado de Hi-Hat en Batería 'Elite': por defecto
pref-elite-hihat-cross = Lado de Hi-Hat en Batería 'Elite': mano cruzada
pref-elite-hihat-open = Lado de Hi-Hat en Batería 'Elite': mano abierta
pref-elite-ride-default = Lado de Ride en Batería 'Elite': por defecto
pref-elite-ride-far = Lado de Ride en Batería 'Elite': crash lejano
pref-elite-ride-near = Lado de Ride en Batería 'Elite': crash cercano
pref-waveform = Forma de onda
pref-lefty-flip = Modo zurdo

pref-ogg-quality = Calidad de OGG Vorbis
pref-decrypt-silent = Interpretar MOGGs con errores de desencriptación como silenciosos en lugar de errores

pref-magma-warning =
  ¡Advertencia! Desactivar Magma puede resultar en archivos de Rock Band que crasheen el juego,
  porque Onyx no verifica todos los errores como lo hace Magma.
  ¡Por favor verifica que tus charts funcionan antes de distribuir!

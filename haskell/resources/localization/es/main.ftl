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
version-latest = Esta es la versión más actual.
version-prerelease = Versión beta

function-load = Cargar una canción
function-batch = Recompilar canciones
function-quick = Convertidor/empaquetador rápido
function-other = Otras herramientas

welcome-header =
  Herramientas <color=magenta>Onyx</color> para juegos de música, versión {$version}
  Elige una opción de abajo para empezar.

welcome-load =
  cambiar una canción de un juego a otro
  cambiar la velocidad de la canción, reasignar instrumentos
  herramienta para previsualizar charts
welcome-batch =
  "Cargar una canción" pero para varias canciones
welcome-quick =
  editar canciones sin cambiarlas de juego
  crear paquetes o extraer paquetes
  cambiar formato de consola, e.g. <color=green>360</color> -> <color=cyan>PS3</color>
welcome-other =
  herramientas de MIDI/donido
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

menu-help = &Help
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
  Ajusta la velocidad de la chart y su audio (sin cambiar el tono).

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
  ¡Asegúrate de combinar canciones a paquetes (dentro de 'Otras herramientas') antes de jugar! Cargar más de 16 paquetes corrompe tu partida de GH2.
warning-gh2-packs-long =
  ¡Advertencia! Cuando estas importando canciones a Guitar Hero II en Xbox 360, *necesitas* combinarlas a paquetes (dentro de 'Otras herramientas').
  ¡Cargando mas de 16 paquete resultara en canciones erróneas y corrompiera tu partida!
warning-gh3-cache =
  ¡Asegúrate de crear un caché de canciones para GH3 (dentro de 'Otras herramientas') de todas tus customs y DLC! Esto es requerido para cargar varias canciones.
warning-ghwor-cache =
  ¡Asegúrate de crear un caché de para GH:WoR (dentro de 'Otras herramientas') de todas tus customs y DLC! Esto es requerido para cargar varias canciones.
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

drums-mode-4lane = Batería de 4 lineas
drums-mode-5lane = Batería de 5 lineas
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

toggle-ps2-practice-audio = Generar sonido de modo practica para PS2

template-runner-tooltip =
  Estructura de ubicación para archivos nuevos.
    %input_dir% - carpeta que contiene la entrada
    %input_base% - nombre de archivo de entrada sin extensión
    %modifiers% - características distintivas e.g. modificador de velocidad
    %title% - titulo desde los metadatos de la canción (incluyendo modificadores)
    %artist% - artista desde los metadatos de la canción
    %album% - album desde los metadatos de la canción
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
  .tooltip = Si activas esta opción, se agregarán marcadores de Tom a todas las notas al importar un chart de FoF/PS/CH donde no se detecte Batería Pro.
batch-tab-gh-drums-5lane = Batería Pro a 5 lineas
batch-tab-gh-drums-4lane = Batería Pro a 4 lineas
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
  .tooltip = Asegura que fácil/medio usen 3/4 trastes respectivamente en cinco lineas.

quick-convert-rb-black-venue = VENUE negra
  .tooltip = Reemplaza la pista de VENUE con una que intenta imitar un fondo negro. Funciona en canciones previas a RB3 o canciones en formato RBN2; no funciona con canciones de RB3 en formato .milo.
quick-convert-rb-no-overdrive = Sin sobrecargar
  .tooltip = Elimina todas las frases de sobrecargar en todos los instrumentos.
quick-convert-rb-no-lanes-gbk = Sin carriles (G/B/K)
  .tooltip = Elimina carriles de tremolo y trino de guitarra/bajo/teclado y sus versiones de Pro. No elimina glissandos de Teclado Pro.
quick-convert-rb-no-lanes-drums = Sin carriles (batería)
  .tooltip = Elimina carriles de redobles de uno o dos parches de Batería.
quick-convert-rb-no-drum-fills = Sin rellenos en batería
  .tooltip = Elimina los rellenos de activación de batería. No elimina los finales apoteósicos. Esta función es principalmente para grabar vídeos.
quick-convert-rb-force-22-protar = Forzar Protarra de 22 trastes
  .tooltip = En canciones con Guitarra/Bajo que tienen versiones distintas para Mustang y Squier, se elimina la versión de Mustang, lo cual deja solo la versión de Squier. Esta función es principalmente para grabar vídeos.
quick-convert-rb-unmute-22-protar = Unmute >22-fret protar
  .tooltip = Any muted notes over fret 22 on Pro Guitar/Bass are unmuted. This allows you to pass them through Magma by muting, and then unmute them to record videos. Note, such notes cannot be hit by normal input.
quick-convert-rb-decompress-milo = Decompress .milo_*
  .tooltip = Converts any .milo_xbox, etc. files to uncompressed form. ForgeTool may require this step to read the files for RB4 conversion.
quick-convert-rb-author-to-tag = Author to DTA tag
  .tooltip = Copies author information from the C3 Magma comment format to the RB2/RB3 Deluxe game-readable format.
quick-convert-rb-strip-title-tags = Strip title tags
  .tooltip = Removes the following tags from song titles: (2x Bass Pedal), and author signatures such as (Z) (X) (O) etc.
quick-convert-rb-note-shuffle = Note shuffle
  .tooltip = Shuffles notes on 5-fret and drum parts.
quick-convert-rb-decrypt-moggs = Desencriptar MOGGs
  .tooltip = Decrypt all MOGG files. Note that unencrypted MOGGs will not work on PS3 RB3 without RB3Enhanced.

quick-convert-ch-chart-to-midi = .chart to MIDI
  .tooltip = Converts all .chart files to .mid.
quick-convert-ch-ps-tap-open = PS tap/open format
  .tooltip = Encodes five-fret tap and open notes using the older Phase Shift format (System Exclusive messages). This will pull back open extended sustains to not overlap other notes, and remove open notes in chords.
quick-convert-ch-audio-ogg = Audio to .ogg
  .tooltip = Reencode audio files as Ogg Vorbis if they are not already.
quick-convert-ch-audio-opus = Audio to .opus
  .tooltip = Reencode audio file as Opus if they are not already.
quick-convert-ch-real-drums-to-elite = Real Drums to Elite
  .tooltip = If a song has Phase Shift Real Drums charted, adds a conversion to Elite Drums.

quick-convert-mode-in-place = Transform in place: replace input files with new versions
quick-convert-mode-one-to-one = One to one: each input file produces a new output file
quick-convert-mode-make-packs = Make packs: combine songs up to a maximum file size
quick-convert-mode-make-songs = Make songs: produce a single file for each song
quick-convert-mode-make-dolphin = Make Dolphin (Wii) pack

quick-convert-format-con = CON (360)
quick-convert-format-live = LIVE (360)
quick-convert-format-pkg = PKG (PS3)
quick-convert-format-folders = Carpetas (PS3)

quick-convert-encrypt-edat = Encriptar .mid.edat
quick-convert-for-ps3-files = For PS3 files:

quick-convert-folder-keep = Keep original USRDIR subfolders
quick-convert-folder-keep-any = Keep original USRDIR subfolders (if any)
quick-convert-folder-combine = Combine into one new USRDIR subfolder
quick-convert-folder-combine-per-file = Combine into one new USRDIR subfolder per file
quick-convert-folder-combine-per-pack = Combine into one new USRDIR subfolder per pack
quick-convert-folder-each = Each song gets a new USRDIR subfolder
quick-convert-folder-custom-initial = Custom USRDIR subfolder...
quick-convert-folder-custom = Custom folder: {$folder}
quick-convert-folder-custom-prompt = Enter custom USRDIR subfolder for your pack. (A number will be appended if creating more than one pack.)

quick-convert-max-pack-size = Max Pack Size (MiB)
quick-convert-invalid-pack-size = Invalid pack size
quick-convert-no-songs-added = No songs added
quick-convert-make-packs-count =
  {$count ->
    [one] Make {$count} pack
   *[other] Make {$count} packs
  }
quick-convert-unsupported-format = Unsupported format: transform in place supports CON/LIVE/PKG
quick-convert-empty = (empty)
quick-convert-songs-incl = {$count} songs incl. {$title}
quick-convert-save-pack-file = Save pack file
quick-convert-save-pack-template = Select template for packs (a number will be added for each pack)
quick-convert-pack-number = Pack #{$number}

quick-convert-fof-folder = folder format
quick-convert-fof-sng = .sng format
quick-convert-fof-encore = Encore format
quick-convert-mode-to-folders = To folders: save each song as a folder
quick-convert-mode-to-sng = To SNG: save each song as an .sng file

quick-convert-ch-warning-base = This will modify the selected songs, and the old versions will be deleted.
quick-convert-ch-warning-encore =
  WARNING: This will convert Encore-format songs into CH-compatible ones, removing the original files.
  Select a different mode if you want to keep them.
quick-convert-ch-ops-header = Based on the filename template, these operations will be performed:
quick-convert-ch-op-overwrite =
  {$songs ->
    [one] - {$songs} song will be overwritten with a new version
   *[other] - {$songs} songs will be overwritten with new versions
  }
quick-convert-ch-op-convert =
  {$songs ->
    [one] - {$songs} song will be converted to a new version
   *[other] - {$songs} songs will be converted to new versions
  }
quick-convert-ch-op-convert-delete =
  {$songs ->
    [one] - {$songs} song will be converted to a new version, and the old version will be deleted
   *[other] - {$songs} songs will be converted to new versions, and the old versions will be deleted
  }
quick-convert-ch-ops-encore = WARNING: Some Encore-format songs will be removed/replaced by CH-compatible ones

quick-convert-delete-originals = Delete original files
quick-convert-dolphin-preview-audio = Try to generate preview audio
quick-convert-task-make-dolphin = Make Dolphin pack

task-legacy-con-to-pkg-rb2 = Convert to PKG (RB2)
task-legacy-con-to-pkg-rb3 = Convert to PKG (RB3)

pack-game-from-first = Game (from first in list):
pack-game = Game:
pack-save-stfs = Save STFS file
pack-name = Pack name
pack-description = Pack description
pack-make-con = Make CON pack (RB3/RB2)
pack-make-live = Make LIVE pack (all other games)
pack-make-extracted = Make extracted folder
pack-save-as = Save pack as…
pack-task-extracted = Save pack as extracted contents: {$path}
pack-task-con = Save pack as CON: {$path}
pack-task-live = Save pack as LIVE: {$path}

button-select-folder = Select folder…

song-count =
  {$count ->
    [one] {$count} song
   *[other] {$count} songs
  }

## misc tools

misc-tab-midi-stuff = MIDI stuff
misc-tab-mogg-vgs-fsb = MOGG/VGS/FSB
misc-tab-lipsync = Lipsync
misc-tab-dry-vox = Dry vox
misc-tab-milo = .milo
misc-tab-rb3-cache = RB3 cache
misc-tab-gh3-cache = GH3 cache
misc-tab-ghwor-cache = GH:WoR cache

misc-button-extract-milo = Extract .milo to folder
misc-button-repack-milo = Repack .milo from folder
misc-button-gh3-cache-360 = Create GH3 cache (360)
misc-button-gh3-cache-ps3 = Create GH3 cache (PS3)
misc-button-gh3-cache-install-rpcs3 = Install GH3 cache (RPCS3)
misc-button-ghwor-cache-360 = Create GH:WoR cache (360)
misc-button-ghwor-cache-ps3 = Create GH:WoR cache (PS3)
misc-button-ghwor-cache-install-rpcs3 = Install GH:WoR cache (RPCS3)

misc-task-make-gh3-cache = Make GH3 cache file
misc-task-install-gh3-cache = Install GH3 cache file
misc-task-make-ghwor-cache = Make GH:WoR cache file
misc-task-install-ghwor-cache = Install GH:WoR cache file

label-midi-or-chart = MIDI or .chart
midi-load-dialog = Load MIDI or .chart file
midi-button-reduce = Fill in lower difficulties and drum animations
midi-task-reduce = Reduce MIDI: {$path}
midi-button-prokeys = Find hanging Pro Keys notes
midi-task-prokeys = Pro Keys range check: {$path}
midi-button-reaper = Make REAPER project with RB template
midi-task-reaper = Make REAPER project: {$path}
midi-button-chart-to-midi = Clean convert .chart to MIDI
midi-task-chart-to-midi = Convert to MIDI: {$path}
midi-button-batch = Batch
midi-task-chart-batch = Convert .chart to MIDI (batch): {$path}

label-con-folder = CON folder
label-song-cache = Song cache
cache-load-dialog = Load RB3 song cache
cache-button-apply = Apply numeric IDs from song cache
cache-task = Hardcode song cache: {$path}

util-button-preview = Build web preview
util-button-reduce = Produce MIDI with automatic reductions
util-save-midi-dialog = Save MIDI file
util-task-prokeys = Pro Keys range check

mogg-button-ogg = Ogg to MOGG, no re-encode ({
  $channels ->
    [one] {$channels} channel
   *[other] {$channels} channels
  })
mogg-button-combine = Combine into MOGG ({
  $channels ->
    [one] {$channels} channel
   *[other] {$channels} channels
  })
mogg-button-vgs = Combine into VGS ({
  $channels ->
    [one] {$channels} channel
   *[other] {$channels} channels
  })
mogg-button-fsb3 = Make FSB3-XMA1 ({
  $streams ->
    [one] {$streams} stream
   *[other] {$streams} streams
  }, {
  $channels ->
    [one] {$channels} channel
   *[other] {$channels} channels
  } total)
mogg-button-fsb4 = Make FSB4-XMA2 ({
  $channels ->
    [one] {$channels} channel
   *[other] {$channels} channels
  })
mogg-task-ogg = Ogg to MOGG
mogg-task-mogg = MOGG file creation
mogg-task-vgs = VGS file creation
mogg-task-fsb3 = FSB3 file creation
mogg-task-fsb4 = FSB4 file creation
audio-format-channels =
  {$format}, {$channels ->
    [one] {$channels} channel
   *[other] {$channels} channels
  }
audio-duration =
  {$duration} ({$frames ->
    [one] {$frames} frame
   *[other] {$frames} frames
  } @ {$rate} Hz)

label-midi-file = MIDI file
label-audio-file = Audio file
label-transition-ms = Transition (ms)

dryvox-blank = (blank)

misc-button-make-voc = Make GH2 .voc from PART VOCALS
misc-button-make-milo = Make RB3 .milo_*
  .tooltip = Create a RB3 .milo_* file from the first present out of: LIPSYNC* tracks, HARM* tracks, or PART VOCALS.
misc-button-update-milo = Update RB3/TBRB .milo_*
  .tooltip = Update a RB3 .milo_* file from LIPSYNC* tracks, HARM* tracks, or PART VOCALS. Or, update a TBRB .milo_* file from LIPSYNC_[beatle] tracks.
misc-button-create-lipsync = Create *.lipsync
  .tooltip = Export all LIPSYNC* and LIPSYNC_[beatle] tracks to .lipsync files, ready to be inserted into .milo_* files.
misc-button-vocal-lipsync-rb3 = Turn vocal tracks into LIPSYNC* tracks for RB3
  .tooltip = Uses HARM* or PART VOCALS to produce LIPSYNC{"{"}1-3{"}"} tracks containing viseme information, using the standard RB viseme set.
misc-button-vocal-lipsync-tbrb = Turn vocal tracks into LIPSYNC* tracks for TBRB
  .tooltip = Uses HARM* or PART VOCALS to produce LIPSYNC{"{"}1-3{"}"} tracks containing viseme information, using the Beatles RB viseme set.
misc-button-dryvox-sine = Make sine wave dry vox from MIDI
misc-button-dryvox-clipped = Make clipped dry vox from MIDI and audio

misc-task-extract-milo = Extract {$path}
misc-task-repack-milo = Repack {$path}
misc-task-make-voc = Make GH2 .voc: {$path}
misc-task-make-milo = Create .milo from MIDI lipsync: {$path}
misc-task-update-milo = Update .milo with MIDI lipsync: {$path}
misc-task-export-lipsync = Export .lipsync to folder
misc-task-vocal-to-lipsync = Convert vocal tracks to LIPSYNC tracks: {$path}
misc-task-dryvox = {$name}: {$path}

## preferences

button-save = Save
button-start = Start

pref-unlimited-cores = Unlimited cores
pref-core-limit = Core limit
pref-detect-muted = For non-open-note games, convert "muted strums" to Harmonix style
  .tooltip = When checked, sequences of the form "chord, strummed open notes, strummed chord" are assumed to represent muted strums. Instead of always becoming green, these open notes will become the lowest note of the following chord, or of the preceding chord if it is closer to the open notes.
pref-limit-reduced-frets = When generating lower difficulty 5-fret tracks, limit Medium to 4 frets and Easy to 3 frets

pref-magma-require = Magma required
pref-magma-try = Magma optional
pref-magma-disable = Don't use Magma
pref-black-venue = Always export black VENUE track
pref-label-2x = Label 2x kick charts as (2x Bass Pedal) by default
pref-legal-tempos = Modify tempos in RB export to follow Magma rules
  .tooltip = Magma requires MIDI files to use tempos in the range of 40 to 300 BPM. This is a more restricted range than the games actually require.
pref-rb3-encoding-label = RB3 recompile .dta
pref-pack-encoding-label = RB3 pack .dta
encoding-latin1 = Latin-1
encoding-utf8 = UTF-8
encoding-as-is = As is

pref-gh2-offset = GH2 Audio Offset (ms)
  .tooltip = Adjust audio offset only for Guitar Hero II output, intended to compensate for emulator delay. Positive values mean audio will be pulled earlier, to account for delay. Negative values mean audio will be pushed later.
pref-sort-gh2 = Sort GH2 bonus songs when adding to .ARK
pref-artist-sort = For GH2 and GH3, sort by artist first (instead of title)
  .tooltip = This affects GH2 .ARK bonus songs (when the above box is checked) and GH3 DLC (as sorted in the Song Cache file).

pref-dir-ch = Default CH folder
pref-ch-downmix = Mix down stems in CH output
pref-ch-audio-ogg = Output Ogg Vorbis audio
pref-ch-audio-opus = Output Opus audio

pref-dir-rb = Default CON/LIVE folder
pref-trim-xbox = Ensure valid filenames for Xbox 360 USB or HDD drives
  .tooltip = When checked, the filenames of CON or LIVE files will be trimmed to a max of 42 characters, and will also have plus and comma characters replaced (a restriction for files on a 360 hard drive, but not USB).
pref-rb-number-id = Use true number IDs instead of symbols on Xbox 360 RB files
  .tooltip = When checked, Rock Band files will use a random integer for song_id. Otherwise, an 'o' is prefixed to form a symbol, and the game will generate a random ID. A true integer is required for some contexts such as RGH online play and PS3 conversion, and also maintains scores even if the song cache is regenerated.

pref-ps3-encrypt = Encrypt .mid.edat files for PS3
  .tooltip = When checked, .mid.edat files for PS3 customs will be encrypted. This is required for a real console, but optional (and can be less convenient) when playing on emulator.
pref-dir-ps3 = PS3/RPCS3 install folder

pref-dir-wii = Default Wii .app folder

pref-msaa-none = No MSAA
pref-msaa-2x = MSAA 2x
pref-msaa-4x = MSAA 4x
pref-msaa-8x = MSAA 8x
pref-msaa-16x = MSAA 16x
pref-fxaa = FXAA
pref-fps = Frames per second
pref-elite-hihat-default = Elite drums hihat side: default
pref-elite-hihat-cross = Elite drums hihat side: cross hand
pref-elite-hihat-open = Elite drums hihat side: open hand
pref-elite-ride-default = Elite drums ride side: default
pref-elite-ride-far = Elite drums ride side: far crash
pref-elite-ride-near = Elite drums ride side: near crash
pref-waveform = Audio waveform
pref-lefty-flip = Lefty flip

pref-ogg-quality = OGG Vorbis quality
pref-decrypt-silent = Treat undecryptable MOGGs as silent instead of an error

pref-magma-warning =
  Warning! Disabling Magma can result in Rock Band files that crash the game,
  as Onyx does not yet check all of the error cases that Magma does.
  Please test any charts thoroughly before distributing to others!

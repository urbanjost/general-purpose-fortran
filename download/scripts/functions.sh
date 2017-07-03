####################################################################################################################################
#                                                                       
#   XX                                      X                           
#  X                               X                                    
#  X                               X                                    
# XXXX   XX  XX  XX XX    XXXXX   XXXX    XXX     XXXXX  XX XX    XXXXX 
#  X      X   X   XX  X  X     X   X        X    X     X  XX  X  X     X
#  X      X   X   X   X  X         X        X    X     X  X   X   XXX   
#  X      X   X   X   X  X         X        X    X     X  X   X      XX 
#  X      X  XX   X   X  X     X   X  X     X    X     X  X   X  X     X
# XXXX     XX XX XXX XXX  XXXXX     XX    XXXXX   XXXXX  XXX XXX  XXXXX 
#                                                                       
####################################################################################################################################
BASENAME(){
#@(#) emulate basename(1) but hopefully quicker because no process is spawned
TMPNAME="${1:-''}"
SUFFIX="${2:-''}"
TMPNAME=${TMPNAME##*/}                              # get rid of leading directories if any
[ "$SUFFIX" != '' ] && TMPNAME=${TMPNAME%$SUFFIX}   # remove suffix
echo "$TMPNAME"
}
####################################################################################################################################

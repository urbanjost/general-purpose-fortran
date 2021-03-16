with open( 'testfile.dat', 'w' ) as fh:
    for i in range(300):
        fh.write( '{0:03d}.'.format(i+1) )

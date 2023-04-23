/**
 * An abstract class for chess pieces
 */
public abstract class Piece implements Moveable{
    // position of this piece on chess board
    private int x; // x-axis
    private int y; // y-axis
    private final boolean red; // a boolean value indicating if the piece is red

    // constructor
    public Piece(int x, int y, boolean red){
	this.x = x;
	this.y = y;
	this.red = red;
    }

    // get the x value
    public int getX(){
	return this.x;
    }
    // get the y value
    public int getY(){
	return this.y;
    }
    // return true if the piece is red, false otherwise
    public boolean isRed(){
	return this.red;
    }

    // return true if the given piece has the same color as the current one, false otherwise
    public boolean sameColor(Piece p){
	// check null values
	if(p == null)
	    return false;
	return(isRed() == p.isRed());
    }

    // TODO: return true if the piece has already crossed the river, false otherwise
    public boolean riverCrossed(){
	// ...
	return false;
    }

    // TODO: return true if it is in its corresponding palace.
    public boolean atPalace(){
	return false;
    }

}

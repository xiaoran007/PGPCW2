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

    // return true if the piece has already crossed the river, false otherwise
    public boolean riverCrossed(){
        if (this.red == true){  // red
            if (this.y <= 4){
                return true;
            }
            else {
                return false;
            }
        }
        else {
            if (this.y >= 5){   // black
                return true;
            }
            else {
                return false;
            }
        }
    }

    // return true if it is in its corresponding palace.
    public boolean atPalace(){
        if (this.red == true){  // red
            if ((this.y >= 7 && this.y <= 9) && (this.x >= 3 && this.x <= 5)){
                return true;
            } 
            else {
                return false;
            }
        }
        else {  // black
            if ((this.y >= 0 && this.y <= 2) && (this.x >= 3 && this.x <= 5)){
                return true;
            }
            else {
                return false;
            }
        }
    }

}

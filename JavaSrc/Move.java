/**
 * An  class for movement actions in Chinese Chess
 */
public abstract class Move{

    // movement distance at x-axis
    private int x;
    // movement distance at y-axis 
    private int y;

    // constructor
    public Move(int x, int y){
	this.x = x;
	this.y = y;
    }

    // return the value of x
    public int getX(){
	return this.x;
    }

    // return the value of y
    public int getY(){
	return this.y;
    }
    

    // a method to printout the current move
    public void printMove(){

	// check horizontal movement
	if(x != 0){
	    String horizontal;
	    if(x > 0)
		horizontal = "Right";
	    else
		horizontal = "Left";
	    System.out.print("Move " + horizontal + " " + Math.abs(x) + " Steps ");
	}
	// check vertical movement
	if(y != 0){
	    String vertical;
	    if(y > 0)
		vertical = "Down";
	    else
		vertical = "Up";
	    System.out.print("Move " + vertical + " " + Math.abs(y) + " Steps ");
	}
	System.out.println();
    }

    public abstract boolean canMove(Piece p, ChessBoard board);
    
}

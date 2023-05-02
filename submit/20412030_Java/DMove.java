/**
 * A class for diagonal moves
 */
public class DMove extends Move{

    // constructor
    public DMove(int x, int y){
	super(x, y);
    }
    
    // return true, if a piece can move n points orthogonally
    public boolean canMove(Piece p, ChessBoard board){
	
	// check null values
	if(p == null || board == null)
	    return false;

	// the absolute value of x and y must be the same
	if(Math.abs(getX()) != Math.abs(getY()))
	    return false;
	
	// get the destination position
	int dx = p.getX() + getX();
	int dy = p.getY() + getY();


	// first of all check the destination position is valid
	if(!board.validPosition(dx, dy))
	    return false;
	
	// check if the destination position is unoccupied or occupied by an enemy's piece
	Piece dp = board.getPieceAt(dx, dy);
	if(!p.sameColor(dp)){
	    // one point diagonally, i.e., advisor 
	    if(Math.abs(getX()) == 1)
		return true;
	}

	return false;
    }
    
}

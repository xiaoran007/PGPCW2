/**
 * A class for orthogonal moves
 */
public class OMove extends Move{
    
    // constructor
    public OMove(int x, int y){
	super(x, y);
    }

    // return true, if a piece can move n points orthogonally
    public boolean canMove(Piece p, ChessBoard board){
	// check null values
	if(p == null || board == null)
	    return false;
	// get the destination position
	int dx = p.getX() + getX();
	int dy = p.getY() + getY();

	// first of all check the destination position is valid
	if(!board.validPosition(dx, dy))
	    return false;

	// check if the destination position is unoccupied or occupied by an enemy's piece
	Piece dp = board.getPieceAt(dx, dy);

	if(!p.sameColor(dp))
	    // if it is the case, we also need to check if the path from the piece's current position to its destination position are all unoccupied
	    return board.unoccupied(p.getX(), dx, p.getY(), dy);

	return false;
	
    }
}

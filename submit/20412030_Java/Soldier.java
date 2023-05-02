import java.util.ArrayList;
/**
 * A java class for soldier
 */
public class Soldier extends Piece{

    // constructor
    public Soldier(int x, int y, boolean red){
	super(x, y, red);
    }

    // a method to get all possible moves for a soldier 
    @Override 
    public Move[] getMoves(ChessBoard board){
	ArrayList<Move> mList = new ArrayList<Move>();
	// move forward
	OMove forward;
	
	// if it is a red piece
	if(isRed())
	    forward = new OMove(0, -1);
	else
	    forward = new OMove(0, 1);
	// check if the movement is valid
	if(forward.canMove(this, board))
	    mList.add(forward);
	
	// check if the piece has crossed the river
	if(riverCrossed()){
	    // move left
	    OMove left = new OMove(-1, 0);
	    // if this piece can move left
	    if(left.canMove(this, board))
		mList.add(left);
	    // move right
	    OMove right = new OMove(1, 0);
	    // if this piece can move right
	    if(right.canMove(this, board))
		mList.add(right);
	}
	Move[] ms = new Move[mList.size()];
	mList.toArray(ms);
	return ms;
    }
    
}

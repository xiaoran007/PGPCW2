import java.util.ArrayList;
/**
 * A java class for guard
 */
public class Guard extends Piece{

    // constructor
    public Guard(int x, int y, boolean red){
	super(x, y, red);
    }
    
    @Override
    public Move[] getMoves(ChessBoard board){
	ArrayList<Move> mList = new ArrayList<Move>();
	// a guard can move towards top-left, top-right, bottom-left and bottom-right, but within its palace
	// move towards top-left
	DMove topleft = new DMove(-1, -1);
	// if this move can be performed within the palace
	if(topleft.canMove(this, board) && (new Guard(getX()-1, getY()-1, isRed())).atPalace())
	    mList.add(topleft);
	// move towards top-right
	DMove topright = new DMove(1, -1);
	// if this move can be performed within the palace
	if(topright.canMove(this, board) && (new Guard(getX()+1, getY()-1, isRed())).atPalace())
	    mList.add(topright);
	// move towards bottom-left
	DMove bottomleft = new DMove(-1, 1);
	// if this move can be performed within the palace
	if(bottomleft.canMove(this, board) && (new Guard(getX()-1, getY()+1, isRed())).atPalace())
	    mList.add(bottomleft);
	// move towards bottom-right
	DMove bottomright = new DMove(1, 1);
	// if this move can be performed within the palace
	if(bottomright.canMove(this, board) && (new Guard(getX()+1, getY()+1, isRed())).atPalace())
	    mList.add(bottomright);

	Move[] ms = new Move[mList.size()];
	mList.toArray(ms);
	return ms;
    }
}

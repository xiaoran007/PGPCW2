import java.util.ArrayList;
public class Elephant extends Piece{
    public Elephant(int x, int y, boolean red){
        super(x, y, red);
    }

    @Override
    public Move[] getMoves(ChessBoard board){
        ArrayList<Move> mList = new ArrayList<Move>();
        DMove upRight = new DMove(2, -2);
        if (upRight.canMove(this, board)){
            DMove upRight1 = new DMove(1, -1);
            if (upRight1.canMove(this, board) && !(new Elephant(getX() + 2, getY() - 2, isRed()).riverCrossed())){
                mList.add(upRight);
            }
        }

        DMove upLeft = new DMove(-2, -2);
        if (upLeft.canMove(this, board)){
            DMove upLeft1 = new DMove(-1, -1);
            if (upLeft1.canMove(this, board) && !(new Elephant(getX() - 2, getY() - 2, isRed()).riverCrossed())){
                mList.add(upLeft);
            }
        }

        DMove downRight = new DMove(2, 2);
        if (downRight.canMove(this, board)){
            DMove downRight1 = new DMove(1, 1);
            if (downRight1.canMove(this, board) && !(new Elephant(getX() + 2, getY() + 2, isRed()).riverCrossed())){
                mList.add(downRight);
            }
        }

        DMove downLeft = new DMove(-2, 2);
        if (downLeft.canMove(this, board)){
            DMove downLeft1 = new DMove(-1, 1);
            if (downLeft1.canMove(this, board) && !(new Elephant(getX() - 2, getY() + 2, isRed()).riverCrossed())){
                mList.add(downLeft);
            }
        }

        Move[] ms = new Move[mList.size()];
        mList.toArray(ms);
        return ms;
    }
    
}

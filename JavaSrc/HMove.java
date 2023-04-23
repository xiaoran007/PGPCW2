public class HMove extends Move {
    public HMove(int x, int y){
        super(x, y);
    }

    public boolean canMove(Piece p, ChessBoard board){
        if (p == null || board == null){
            return false;
        }
        // check value
        if ((Math.abs(getX()) != 2 * Math.abs(getY())) || (2 * Math.abs(getX()) != Math.abs(getY()))){
            return false;
        }

        int dx = p.getX() + getX();
        int dy = p.getY() + getY();

        if(!board.validPosition(dx, dy)){
            return false;
        }

        Piece dp = board.getPieceAt(dx, dy);
        // up
	    if (getY() == -2){
            if (getX() == 1){ // up and right
                OMove up = new OMove(0, -1);
                if (up.canMove(p, board)){
                    DMove right = new DMove(1, -1);
                    if (right.canMove(new Horse(p.getX(), p.getY() - 1, p.isRed()), board)){
                        return true;
                    }
                }
            }
            else {  // up and left
                OMove up = new OMove(0, -1);
                if (up.canMove(p, board)){
                    DMove left = new DMove(-1, -1);
                    if (left.canMove(new Horse(p.getX(), p.getY() - 1, p.isRed()), board)){
                        return true;
                    }
                }
            }
        }
        // down
        else if(getY() == 2){
            if (getX() == 1){   // down and right
                OMove down = new OMove(0, 1);
                if (down.canMove(p, board)){
                    DMove right = new DMove(1, 1);
                    if (right.canMove(new Horse(p.getX(), p.getY() + 1, p.isRed()), board)){
                        return true;
                    }
                }
            }
            else {  // down and left
                OMove down = new OMove(0, 1);
                if (down.canMove(p, board)){
                    DMove left = new DMove(-1, 1);
                    if (left.canMove(new Horse(p.getX(), p.getY() + 1, p.isRed()), board)){
                        return true;
                    }
                }
            }
        }
        // left or right
        else if(getY() == -1){
            if (getX() == -2){  // left and up
                OMove left = new OMove(-1, 0);
                if (left.canMove(p, board)){
                    DMove up = new DMove(-1, -1);
                    if (up.canMove(new Horse(p.getX() - 1, p.getY(), p.isRed()), board)){
                        return true;
                    }
                }
            }
            else {  // right and up
                OMove right = new OMove(1, 0);
                if (right.canMove(p, board)){
                    DMove up = new DMove(1, -1);
                    if (up.canMove(new Horse(p.getX() + 1, p.getY(), p.isRed()), board)){
                        return true;
                    }
                }
            }
        }
        else if (getY() == 1){
            if (getX() == -2){  // left and down
                OMove left = new OMove(-1, 0);
                if (left.canMove(p, board)){
                    DMove down = new DMove(-1, 1);
                    if (down.canMove(new Horse(p.getX() - 1, p.getY(), p.isRed()), board)){
                        return true;
                    }
                }
            }
            else {  // right and down
                OMove right = new OMove(1, 0);
                if (right.canMove(p, board)){
                    DMove down = new DMove(1, 1);
                    if (down.canMove(new Horse(p.getX() + 1, p.getY(), p.isRed()), board)){
                        return true;
                    }
                }
            }
        }
        return false;
    }
    
}

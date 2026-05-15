package konane;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class Controller {

    @FXML
    private GridPane tabuleiro;

    private GameAPI game;

    @FXML
    public void initialize() {

        game = new GameAPI(6,6);

        renderBoard();
    }

    private void renderBoard() {

        tabuleiro.getChildren().clear();

        for (int r = 0; r < game.getRows(); r++) {

            for (int c = 0; c < game.getCols(); c++) {

                Button botao = new Button();

                botao.setPrefSize(80,80);

                botao.setStyle("""
                    -fx-background-color: lightgray;
                    -fx-border-color: black;
                """);

                String stone =
                        game.getStone(r,c);

                String imagePath = "";

                switch (stone) {

                    case "BLACK":
                        imagePath = "/black.png";
                        break;

                    case "WHITE":
                        imagePath = "/white.png";
                        break;
                }

                if (!imagePath.isEmpty()) {

                    Image img =
                            new Image(
                                    getClass()
                                            .getResourceAsStream(imagePath)
                            );

                    ImageView view =
                            new ImageView(img);

                    view.setFitWidth(50);
                    view.setFitHeight(50);

                    botao.setGraphic(view);
                }

                if (game.isSelected(r,c)) {

                    botao.setStyle("""
                        -fx-background-color: yellow;
                        -fx-border-color: red;
                        -fx-border-width: 3;
                    """);
                }

                final int row = r;
                final int col = c;

                botao.setOnAction(e -> {

                    game.select(row,col);

                    renderBoard();
                });

                tabuleiro.add(botao, c, r);
            }
        }
    }
}
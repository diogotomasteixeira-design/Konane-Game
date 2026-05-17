package konane;

import javafx.application.Application;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

public class GUI extends Application {

    @Override
    public void start(Stage stage) throws Exception {
        VBox root = new VBox(15);
        root.setPadding(new Insets(20));
        root.setAlignment(Pos.CENTER);
        root.setStyle("-fx-background-color: #D3D3D3;");

        Label lblCronometro = new Label("Tempo Restante: --s");
        lblCronometro.setStyle("""
            -fx-font-size: 18px;
            -fx-font-weight: bold;
            -fx-text-fill: #e74c3c;
            -fx-background-color: #636669;
            -fx-padding: 5 15 5 15;
            -fx-background-radius: 5;
        """);

        GridPane tabuleiro = new GridPane();
        tabuleiro.setAlignment(Pos.CENTER);
        tabuleiro.setHgap(5);
        tabuleiro.setVgap(5);

        HBox painelBotoes = new HBox(15);
        painelBotoes.setAlignment(Pos.CENTER);

        Button btnParar = new Button("Parar Capturas");
        Button btnUndo = new Button("Desfazer Jogada (Undo)");
        Button btnReiniciar = new Button("Reiniciar Jogo");

        String estiloBotao = """
            -fx-font-size: 14px;
            -fx-font-weight: bold;
            -fx-background-color: #ecf0f1;
            -fx-text-fill: #2c3e50;
            -fx-border-radius: 5;
            -fx-background-radius: 5;
            -fx-padding: 8 15 8 15;
        """;

        btnParar.setStyle(estiloBotao);
        btnUndo.setStyle(estiloBotao);
        btnReiniciar.setStyle(estiloBotao);

        painelBotoes.getChildren().addAll(btnParar, btnUndo, btnReiniciar);
        root.getChildren().addAll(lblCronometro, tabuleiro, painelBotoes);

        Controller controller = new Controller(tabuleiro, btnParar, btnUndo, btnReiniciar, lblCronometro);
        controller.init();

        Scene scene = new Scene(root);
        stage.setTitle("Kōnane Game");
        stage.setScene(scene);
        stage.show();
    }
}
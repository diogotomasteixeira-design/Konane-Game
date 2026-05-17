package konane;

import javafx.animation.Animation;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Platform;
import javafx.scene.control.Alert;
import javafx.scene.control.Button;
import javafx.scene.control.ChoiceDialog;
import javafx.scene.control.Label;
import javafx.scene.control.TextInputDialog;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import javafx.util.Duration;
import java.util.Arrays;
import java.util.List;

public class Controller {

    private final GridPane tabuleiro;
    private final Button btnParar;
    private final Button btnUndo;
    private final Button btnReiniciar;
    private final Label lblCronometro;
    private GameAPI game;

    private int tempoLimitePorJogada;
    private int tempoRestante;
    private Timeline cronometro;

    public Controller(GridPane tabuleiro, Button btnParar, Button btnUndo, Button btnReiniciar, Label lblCronometro) {
        this.tabuleiro = tabuleiro;
        this.btnParar = btnParar;
        this.btnUndo = btnUndo;
        this.btnReiniciar = btnReiniciar;
        this.lblCronometro = lblCronometro;
    }

    public void init() {
        List<String> opcoesTamanho = Arrays.asList("4x4", "6x6", "8x8", "10x10");
        ChoiceDialog<String> dialogTamanho = new ChoiceDialog<>("6x6", opcoesTamanho);
        dialogTamanho.setTitle("Configuração do Tabuleiro");
        dialogTamanho.setHeaderText("Escolha as proporções do tabuleiro de Kōnane:");
        dialogTamanho.setContentText("Dimensões:");

        String tamanhoEscolhido = dialogTamanho.showAndWait().orElse("6x6");
        int linhas = Integer.parseInt(tamanhoEscolhido.split("x")[0]);
        int colunas = Integer.parseInt(tamanhoEscolhido.split("x")[1]);

        List<String> opcoesDificuldade = Arrays.asList("1 - Fácil (Aleatório)", "2 - Médio (Guloso/Max Jumps)");
        ChoiceDialog<String> dialogDificuldade = new ChoiceDialog<>("1 - Fácil (Aleatório)", opcoesDificuldade);
        dialogDificuldade.setTitle("Configuração da IA");
        dialogDificuldade.setHeaderText("Escolha o nível de dificuldade do Computador:");
        dialogDificuldade.setContentText("Dificuldade:");

        String diffEscolhida = dialogDificuldade.showAndWait().orElse("1");
        int dificuldade = diffEscolhida.startsWith("2") ? 2 : 1;

        TextInputDialog dialogTempo = new TextInputDialog("30");
        dialogTempo.setTitle("Configuração do Cronómetro");
        dialogTempo.setHeaderText("Defina o tempo limite por jogada (em segundos):");
        dialogTempo.setContentText("Segundos:");

        String tempoIntroduzido = dialogTempo.showAndWait().orElse("30");
        try {
            tempoLimitePorJogada = Integer.parseInt(tempoIntroduzido);
        } catch (NumberFormatException e) {
            tempoLimitePorJogada = 30;
        }

        game = new GameAPI(linhas, colunas, dificuldade);

        btnParar.setOnAction(e -> {
            game.stopCapturing();
            renderBoard();
            if (game.currentPlayer().equals("WHITE")) {
                ejecutarTurnoComputador();
            }
        });

        btnUndo.setOnAction(e -> {
            game.undo();
            reiniciarCronometro();
            renderBoard();
        });

        btnReiniciar.setOnAction(e -> {
            game.reset();
            reiniciarCronometro();
            renderBoard();
        });

        inicializarCronometro();
        renderBoard();
    }

    private void inicializarCronometro() {
        tempoRestante = tempoLimitePorJogada;
        cronometro = new Timeline(new KeyFrame(Duration.seconds(1), e -> {
            tempoRestante--;
            atualizarTextoVisual();
            if (tempoRestante <= 0) {
                cronometro.stop();
                executarJogadaForcadaPorTempo();
            }
        }));
        cronometro.setCycleCount(Animation.INDEFINITE);
        cronometro.play();
    }

    private void reiniciarCronometro() {
        if (cronometro != null) {
            cronometro.stop();
        }
        tempoRestante = tempoLimitePorJogada;
        atualizarTextoVisual();
        if (!game.isGameOver() && game.currentPlayer().equals("BLACK")) {
            cronometro.play();
        }
    }

    private void atualizarTextoVisual() {
        if (game.isGameOver()) {
            lblCronometro.setText("Fim de Jogo!");
            lblCronometro.setStyle("-fx-font-size: 18px; -fx-font-weight: bold; -fx-text-fill: #2ecc71; -fx-background-color: #34495e; -fx-padding: 5 15 5 15; -fx-background-radius: 5;");
            return;
        }

        if (game.currentPlayer().equals("BLACK")) {
            lblCronometro.setText("Tempo Restante: " + tempoRestante + "s");
            lblCronometro.setStyle("-fx-font-size: 18px; -fx-font-weight: bold; -fx-text-fill: #e74c3c; -fx-background-color: #636669; -fx-padding: 5 15 5 15; -fx-background-radius: 5;");
        }
    }

    private void executarJogadaForcadaPorTempo() {
        if (game.currentPlayer().equals("BLACK")) {
            game.jogarAleatorioHumano();
            renderBoard();
            if (game.currentPlayer().equals("WHITE")) {
                ejecutarTurnoComputador();
            }
        }
    }

    private void verficarFimDeJogo() {
        if (game.isGameOver()) {
            if (cronometro != null) {
                cronometro.stop();
            }
            Platform.runLater(() -> {
                Alert alert = new Alert(Alert.AlertType.INFORMATION);
                alert.setTitle("Fim de Jogo");
                alert.setHeaderText("O jogo Kōnane terminou!");
                if (game.currentPlayer().equals("BLACK")) {
                    alert.setContentText("Não tens mais jogadas disponíveis. O Computador ganhou!");
                } else {
                    alert.setContentText("O computador ficou sem jogadas disponíveis. Parabéns, ganhaste!");
                }
                alert.showAndWait();
            });
        }
    }

    private void renderBoard() {
        tabuleiro.getChildren().clear();

        if (game.isMultiJumping()) {
            btnParar.setVisible(true);
            btnParar.setText("Parar Capturas (Passar Vez)");
        } else {
            btnParar.setVisible(false);
        }

        atualizarTextoVisual();

        for (int r = 0; r < game.getRows(); r++) {
            for (int c = 0; c < game.getCols(); c++) {

                Button botao = new Button();
                botao.setPrefSize(70, 70);

                botao.setStyle("""
                    -fx-background-color: #636669;
                    -fx-border-color: #2c3e50;
                    -fx-border-width: 1;
                """);

                String stone = game.getStone(r, c);
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
                    Image img = new Image(getClass().getResourceAsStream(imagePath));
                    ImageView view = new ImageView(img);
                    view.setFitWidth(45);
                    view.setFitHeight(45);
                    botao.setGraphic(view);
                }

                if (game.isSelected(r, c)) {
                    botao.setStyle("""
                        -fx-background-color: #f1c40f;
                        -fx-border-color: #e74c3c;
                        -fx-border-width: 3;
                    """);
                }

                final int row = r;
                final int col = c;

                botao.setOnAction(e -> {
                    if (game.currentPlayer().equals("BLACK")) {
                        boolean jogadaValida = game.select(row, col, game.getDifficulty());
                        if (jogadaValida) {
                            reiniciarCronometro();
                            renderBoard();
                            if (game.currentPlayer().equals("WHITE")) {
                                ejecutarTurnoComputador();
                            }
                        }
                    }
                });

                tabuleiro.add(botao, c, r);
            }
        }
        verficarFimDeJogo();
    }

    private void ejecutarTurnoComputador() {
        if (cronometro != null) {
            cronometro.stop();
        }
        game.jogarComputador();
        reiniciarCronometro();
        renderBoard();
    }
}
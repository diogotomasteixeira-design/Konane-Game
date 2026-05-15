package konane;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.Parent;
import javafx.stage.Stage;

public class GUI extends Application {

    @Override
    public void start(Stage stage) throws Exception {

        FXMLLoader loader =
                new FXMLLoader(getClass().getResource("/GUI.fxml"));

        Parent root = loader.load();

        Scene scene = new Scene(root);

        stage.setTitle("Konane");
        stage.setScene(scene);
        stage.show();
    }
}
import java.util.Locale;


public class TestLocales {

    public static void main(String[] args) {
        
        System.out.println("Default Locale: " + Locale.getDefault());
        System.out.println("---");
        
        Locale locale = new Locale("es", "ES");
        System.out.println("locale.getDisplayName(): " + locale.getDisplayName());
        System.out.println("locale.getDisplayCountry(): " + locale.getDisplayCountry());
        System.out.println("locale.getDisplayLanguage(): " + locale.getDisplayLanguage());
        System.out.println("locale.getDisplayVariant(): " + locale.getDisplayVariant());
        System.out.println("---");
    }
}

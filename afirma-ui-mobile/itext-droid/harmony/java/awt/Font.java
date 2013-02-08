package harmony.java.awt;

public class Font {

	public static final int NORMAL = 0;
	public static final int BOLD = 1;
	public static final int ITALIC = 2;

	private String name;
	private int size;

	private boolean bold;
	private boolean italic;

	public Font(String name, int style, int size) {
		this.name = name;

		if ((style & BOLD) == BOLD) {
			bold = true;
		}
		if ((style & ITALIC) == ITALIC) {
			italic = true;
		}

		this.size = size;
	}

	public String getFontName() {
		return name + (isBold() ? " Bold" : "") + (isItalic() ? " Italic" : "");
	}

	public boolean isBold() {
		return bold;
	}

	public boolean isItalic() {
		return italic;
	}

	public String getName() {
		return this.name;
	}

}

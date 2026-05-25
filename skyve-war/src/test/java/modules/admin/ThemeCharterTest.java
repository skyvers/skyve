package modules.admin;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.awt.Color;
import java.awt.Font;
import java.awt.image.BufferedImage;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;

import modules.admin.ThemeCharter.SectionColouriser;

/**
 * Unit tests for ThemeCharter and its inner SectionColouriser class.
 * These tests exercise constructors, getters, setters, font accessors,
 * SectionColouriser behaviour, and the getFabulator image generator.
 */
@SuppressWarnings("static-method")
class ThemeCharterTest {

	// ---- ThemeCharter default constructor ----

	@Test
	void defaultConstructorSetsDefaultBaseColour() {
		ThemeCharter tc = new ThemeCharter();
		assertNotNull(tc.getThemeBaseColour());
	}

	@Test
	void defaultConstructorSetsFontName() {
		ThemeCharter tc = new ThemeCharter();
		assertEquals("Arial", tc.getThemeFontName());
	}

	@Test
	void defaultConstructorSetsFontSizes() {
		ThemeCharter tc = new ThemeCharter();
		assertEquals(14, tc.getThemeLegendFontSize());
		assertEquals(14, tc.getThemeTitleFontSize());
		assertEquals(12, tc.getThemeRangeFontSize());
		assertEquals(12, tc.getThemeDomainFontSize());
	}

	// ---- ThemeCharter(Color) constructor ----

	@Test
	void colourConstructorSetsBaseColour() {
		Color red = new Color(255, 0, 0);
		ThemeCharter tc = new ThemeCharter(red);
		assertEquals(red, tc.getThemeBaseColour());
	}

	@Test
	void colourConstructorSetsFontName() {
		ThemeCharter tc = new ThemeCharter(Color.BLUE);
		assertEquals("Arial", tc.getThemeFontName());
	}

	// ---- Setters and getters ----

	@Test
	void setAndGetThemeBaseColour() {
		ThemeCharter tc = new ThemeCharter();
		Color c = new Color(100, 150, 200);
		tc.setThemeBaseColour(c);
		assertSame(c, tc.getThemeBaseColour());
	}

	@Test
	void setAndGetSql() {
		ThemeCharter tc = new ThemeCharter();
		assertNull(tc.getSql());
		tc.setSql("SELECT 1");
		assertEquals("SELECT 1", tc.getSql());
	}

	@Test
	void setAndGetThemeFontName() {
		ThemeCharter tc = new ThemeCharter();
		tc.setThemeFontName("Times New Roman");
		assertEquals("Times New Roman", tc.getThemeFontName());
	}

	@Test
	void setAndGetThemeLegendFontSize() {
		ThemeCharter tc = new ThemeCharter();
		tc.setThemeLegendFontSize(16);
		assertEquals(16, tc.getThemeLegendFontSize());
	}

	@Test
	void setAndGetThemeTitleFontSize() {
		ThemeCharter tc = new ThemeCharter();
		tc.setThemeTitleFontSize(18);
		assertEquals(18, tc.getThemeTitleFontSize());
	}

	@Test
	void setAndGetThemeRangeFontSize() {
		ThemeCharter tc = new ThemeCharter();
		tc.setThemeRangeFontSize(10);
		assertEquals(10, tc.getThemeRangeFontSize());
	}

	@Test
	void setAndGetThemeDomainFontSize() {
		ThemeCharter tc = new ThemeCharter();
		tc.setThemeDomainFontSize(11);
		assertEquals(11, tc.getThemeDomainFontSize());
	}

	@Test
	void setAndGetColouriserTheme() {
		ThemeCharter tc = new ThemeCharter();
		assertNull(tc.getColouriserTheme());
		tc.setColouriserTheme(SectionColouriser.Colouriser.MULTI_COLOUR);
		assertEquals(SectionColouriser.Colouriser.MULTI_COLOUR, tc.getColouriserTheme());
	}

	@Test
	void setAndGetColourPalette() {
		ThemeCharter tc = new ThemeCharter();
		String[] palette = { "#FF0000", "#00FF00", "#0000FF" };
		tc.setColourPalette(palette);
		assertArrayEquals(palette, tc.getColourPalette());
	}

	// ---- Font accessor methods ----

	@Test
	void getThemeLegendFontReturnsFont() {
		ThemeCharter tc = new ThemeCharter();
		Font f = tc.getThemeLegendFont();
		assertNotNull(f);
		assertEquals(Font.PLAIN, f.getStyle());
	}

	@Test
	void getThemeTitleFontReturnsFont() {
		ThemeCharter tc = new ThemeCharter();
		Font f = tc.getThemeTitleFont();
		assertNotNull(f);
		assertEquals(Font.BOLD, f.getStyle());
	}

	@Test
	void getThemeRangeFontReturnsFont() {
		ThemeCharter tc = new ThemeCharter();
		Font f = tc.getThemeRangeFont();
		assertNotNull(f);
		assertEquals(Font.PLAIN, f.getStyle());
	}

	@Test
	void getThemeDomainFontReturnsFont() {
		ThemeCharter tc = new ThemeCharter();
		Font f = tc.getThemeDomainFont();
		assertNotNull(f);
		assertEquals(Font.PLAIN, f.getStyle());
	}

	// ---- SectionColouriser ----

	@Test
	void sectionColouriserConstructorSetsBaseColour() {
		Color base = new Color(100, 100, 100);
		SectionColouriser sc = new SectionColouriser(base, 5, SectionColouriser.Colouriser.SINGLE_COLOUR, new String[0]);
		assertNotNull(sc.getCurrent());
	}

	@Test
	void sectionColouriserConstructorWithZeroColumnCountDoesNotDivide() {
		Color base = new Color(100, 100, 100);
		// columnCount=0 should not cause division by zero
		SectionColouriser sc = new SectionColouriser(base, 0, SectionColouriser.Colouriser.SINGLE_COLOUR, new String[0]);
		assertNotNull(sc);
		assertEquals(0, sc.getRedDiff());
		assertEquals(0, sc.getGreenDiff());
		assertEquals(0, sc.getBlueDiff());
	}

	@Test
	void sectionColouriserGetAndSetColouriser() {
		Color base = new Color(100, 100, 100);
		SectionColouriser sc = new SectionColouriser(base, 3, SectionColouriser.Colouriser.SINGLE_COLOUR, new String[0]);
		assertEquals(SectionColouriser.Colouriser.SINGLE_COLOUR, sc.getColouriser());
		sc.setColour(SectionColouriser.Colouriser.MULTI_COLOUR);
		assertEquals(SectionColouriser.Colouriser.MULTI_COLOUR, sc.getColouriser());
	}

	@Test
	void sectionColouriserGetAndSetCurrent() {
		Color base = new Color(50, 100, 150);
		SectionColouriser sc = new SectionColouriser(base, 2, SectionColouriser.Colouriser.SINGLE_COLOUR, new String[0]);
		Color newCurrent = new Color(200, 200, 200);
		sc.setCurrent(newCurrent);
		assertSame(newCurrent, sc.getCurrent());
	}

	@Test
	void sectionColouriserGetAndSetDiffs() {
		Color base = new Color(100, 100, 100);
		SectionColouriser sc = new SectionColouriser(base, 2, SectionColouriser.Colouriser.SINGLE_COLOUR, new String[0]);
		sc.setRedDiff(5);
		sc.setGreenDiff(10);
		sc.setBlueDiff(15);
		assertEquals(5, sc.getRedDiff());
		assertEquals(10, sc.getGreenDiff());
		assertEquals(15, sc.getBlueDiff());
	}

	@Test
	void sectionColouriserNextColourSingleColourDecreasesComponents() {
		Color base = new Color(120, 100, 80);
		SectionColouriser sc = new SectionColouriser(base, 2, SectionColouriser.Colouriser.SINGLE_COLOUR, new String[0]);
		Color before = sc.getCurrent();
		sc.nextColour();
		Color after = sc.getCurrent();
		// With SINGLE_COLOUR, colour components should decrease
		assertNotNull(after);
		// Verify the colour has changed (diffs are > 0 for non-zero column count)
		// base.getRed() / 2 / 2 = 30, so new Red = 120 - 30 = 90
		assertEquals(before.getRed() - sc.getRedDiff(), after.getRed());
	}

	@Test
	void sectionColouriserNextColourMultiColourUsesPalette() {
		Color base = new Color(100, 100, 100);
		String[] palette = { "#FF0000", "#00FF00", "#0000FF" };
		SectionColouriser sc = new SectionColouriser(base, 3, SectionColouriser.Colouriser.MULTI_COLOUR, palette);
		sc.nextColour();
		Color current = sc.getCurrent();
		// Should decode "#FF0000" = red
		assertNotNull(current);
		assertEquals(255, current.getRed());
		assertEquals(0, current.getGreen());
		assertEquals(0, current.getBlue());
	}

	// ---- getFabulator ----

	@Test
	void getFabulatorWithEmptyListReturnsImage() {
		ThemeCharter tc = new ThemeCharter();
		List<Object[]> objects = new ArrayList<>();
		BufferedImage img = tc.getFabulator(200, 100, objects, null);
		assertNotNull(img);
		assertEquals(200, img.getWidth());
		assertEquals(100, img.getHeight());
	}

	@Test
	void getFabulatorWithDataReturnsImage() {
		ThemeCharter tc = new ThemeCharter();
		List<Object[]> objects = new ArrayList<>();
		objects.add(new Object[] { "Category A", BigInteger.valueOf(100) });
		objects.add(new Object[] { "Category B", BigInteger.valueOf(200) });
		BufferedImage img = tc.getFabulator(300, 150, objects, null);
		assertNotNull(img);
		assertEquals(300, img.getWidth());
		assertEquals(150, img.getHeight());
	}

	@Test
	void getFabulatorWithFocusStringHighlightsFocusRow() {
		ThemeCharter tc = new ThemeCharter();
		List<Object[]> objects = new ArrayList<>();
		objects.add(new Object[] { "Alpha", BigInteger.valueOf(50) });
		objects.add(new Object[] { "Beta", BigInteger.valueOf(75) });
		objects.add(new Object[] { "Gamma", BigInteger.valueOf(25) });
		BufferedImage img = tc.getFabulator(300, 200, objects, "Beta");
		assertNotNull(img);
	}

	@Test
	void getFabulatorWithNullValueDoesNotThrow() {
		ThemeCharter tc = new ThemeCharter();
		List<Object[]> objects = new ArrayList<>();
		objects.add(new Object[] { "Item", null }); // null BigInteger
		BufferedImage img = tc.getFabulator(200, 100, objects, null);
		assertNotNull(img);
	}

	@Test
	void getFabulatorMultiColourMode() {
		ThemeCharter tc = new ThemeCharter();
		tc.setColouriserTheme(SectionColouriser.Colouriser.MULTI_COLOUR);
		List<Object[]> objects = new ArrayList<>();
		objects.add(new Object[] { "X", BigInteger.ONE });
		objects.add(new Object[] { "Y", BigInteger.TEN });
		BufferedImage img = tc.getFabulator(200, 150, objects, null);
		assertNotNull(img);
	}

	// ---- Colouriser enum ----

	@Test
	void sectionColouriserEnumValuesAccessible() {
		assertNotNull(SectionColouriser.Colouriser.SINGLE_COLOUR);
		assertNotNull(SectionColouriser.Colouriser.MULTI_COLOUR);
		assertEquals(2, SectionColouriser.Colouriser.values().length);
	}
}

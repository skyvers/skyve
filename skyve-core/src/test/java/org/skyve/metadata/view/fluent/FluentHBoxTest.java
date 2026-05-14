package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.container.HBox;

public class FluentHBoxTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesHBox() {
		FluentHBox box = new FluentHBox();
		assertNotNull(box.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		HBox hbox = new HBox();
		FluentHBox box = new FluentHBox(hbox);
		assertSame(hbox, box.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void widgetIdReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.widgetId("w1");
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void borderReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.border(true);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void borderTitleReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.borderTitle("Title");
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.pixelWidth(200);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void responsiveWidthReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.responsiveWidth(6);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void smReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.sm(4);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void mdReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.md(4);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void lgReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.lg(4);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void xlReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.xl(4);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void percentageWidthReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.percentageWidth(50);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void minPixelWidthReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.minPixelWidth(100);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void maxPixelWidthReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.maxPixelWidth(500);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelHeightReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.pixelHeight(100);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void percentageHeightReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.percentageHeight(80);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelPaddingReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.pixelPadding(10);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelMemberPaddingReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.pixelMemberPadding(5);
		assertSame(box, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void invisibleConditionNameReturnsSelf() {
		FluentHBox box = new FluentHBox();
		FluentHBox result = box.invisibleConditionName("hiddenCond");
		assertSame(box, result);
	}
}

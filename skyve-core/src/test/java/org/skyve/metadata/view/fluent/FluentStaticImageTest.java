package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.StaticImage;

class FluentStaticImageTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesStaticImage() {
		FluentStaticImage img = new FluentStaticImage();
		assertNotNull(img.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		StaticImage image = new StaticImage();
		FluentStaticImage img = new FluentStaticImage(image);
		assertSame(image, img.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void relativeFileReturnsSelf() {
		FluentStaticImage img = new FluentStaticImage();
		FluentStaticImage result = img.relativeFile("img/logo.png");
		assertSame(img, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthReturnsSelf() {
		FluentStaticImage img = new FluentStaticImage();
		FluentStaticImage result = img.pixelWidth(100);
		assertSame(img, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void responsiveWidthReturnsSelf() {
		FluentStaticImage img = new FluentStaticImage();
		FluentStaticImage result = img.responsiveWidth(6);
		assertSame(img, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void smReturnsSelf() {
		FluentStaticImage img = new FluentStaticImage();
		FluentStaticImage result = img.sm(4);
		assertSame(img, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void mdReturnsSelf() {
		FluentStaticImage img = new FluentStaticImage();
		FluentStaticImage result = img.md(4);
		assertSame(img, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelHeightReturnsSelf() {
		FluentStaticImage img = new FluentStaticImage();
		FluentStaticImage result = img.pixelHeight(80);
		assertSame(img, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void invisibleConditionNameReturnsSelf() {
		FluentStaticImage img = new FluentStaticImage();
		FluentStaticImage result = img.invisibleConditionName("hiddenCond");
		assertSame(img, result);
	}
}

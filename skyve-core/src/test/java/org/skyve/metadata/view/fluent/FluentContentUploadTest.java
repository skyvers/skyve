package org.skyve.metadata.view.fluent;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;
import org.skyve.impl.metadata.view.widget.bound.input.ContentDisplay;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;

class FluentContentUploadTest {
	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentContentUpload().get(), is(notNullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorPreservesInstance() {
		ContentUpload content = new ContentUpload();
		assertEquals(content, new FluentContentUpload(content).get());
	}

	@Test
	@SuppressWarnings("static-method")
	void setsDisplayCaptureEditableMarkupAndSize() {
		ContentUpload content = new FluentContentUpload()
				.display(ContentDisplay.video)
				.capture(ContentCapture.all)
				.editable(true)
				.showMarkup(false)
				.pixelWidth(320)
				.pixelHeight(180)
				.percentageWidth(50)
				.percentageHeight(60)
				.responsiveWidth(6)
				.sm(3)
				.md(4)
				.lg(5)
				.xl(6)
				.minPixelWidth(100)
				.maxPixelWidth(640)
				.minPixelHeight(90)
				.maxPixelHeight(360)
				.get();

		assertEquals(ContentDisplay.video, content.getDisplay());
		assertEquals(ContentCapture.all, content.getCapture());
		assertEquals(Boolean.TRUE, content.getEditable());
		assertEquals(Boolean.FALSE, content.getShowMarkup());
		assertEquals(320, content.getPixelWidth());
		assertEquals(180, content.getPixelHeight());
		assertEquals(50, content.getPercentageWidth());
		assertEquals(60, content.getPercentageHeight());
		assertEquals(6, content.getResponsiveWidth());
		assertEquals(3, content.getSm());
		assertEquals(4, content.getMd());
		assertEquals(5, content.getLg());
		assertEquals(6, content.getXl());
		assertEquals(100, content.getMinPixelWidth());
		assertEquals(640, content.getMaxPixelWidth());
		assertEquals(90, content.getMinPixelHeight());
		assertEquals(360, content.getMaxPixelHeight());
	}

	@Test
	@SuppressWarnings("static-method")
	void fromCopiesValues() {
		ContentUpload source = new ContentUpload();
		source.setBinding("media");
		source.setDisplay(ContentDisplay.image);
		source.setCapture(ContentCapture.camera);
		source.setEditable(Boolean.TRUE);
		source.setShowMarkup(Boolean.TRUE);
		source.setPixelWidth(Integer.valueOf(200));

		ContentUpload copy = new FluentContentUpload().from(source).get();

		assertEquals("media", copy.getBinding());
		assertEquals(ContentDisplay.image, copy.getDisplay());
		assertEquals(ContentCapture.camera, copy.getCapture());
		assertEquals(Boolean.TRUE, copy.getEditable());
		assertEquals(Boolean.TRUE, copy.getShowMarkup());
		assertEquals(200, copy.getPixelWidth());
	}
}

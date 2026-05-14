package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;

public class FluentContentLinkTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesContentLink() {
		FluentContentLink cl = new FluentContentLink();
		assertNotNull(cl.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		ContentLink link = new ContentLink();
		FluentContentLink cl = new FluentContentLink(link);
		assertSame(link, cl.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void valueReturnsSelf() {
		FluentContentLink cl = new FluentContentLink();
		FluentContentLink result = cl.value("someBinding");
		assertSame(cl, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void editableReturnsSelf() {
		FluentContentLink cl = new FluentContentLink();
		FluentContentLink result = cl.editable(false);
		assertSame(cl, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthReturnsSelf() {
		FluentContentLink cl = new FluentContentLink();
		FluentContentLink result = cl.pixelWidth(200);
		assertSame(cl, result);
	}
}

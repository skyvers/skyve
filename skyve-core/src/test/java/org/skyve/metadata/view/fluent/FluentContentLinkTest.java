package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;

class FluentContentLinkTest {

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

	@Test
	@SuppressWarnings("static-method")
	void fromCopiesEditable() {
		ContentLink src = new ContentLink();
		src.setEditable(Boolean.TRUE);
		assertThat(new FluentContentLink().from(src).get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	@SuppressWarnings("static-method")
	void addParameterAddsToList() {
		FluentContentLink cl = new FluentContentLink();
		cl.addParameter(new FluentParameter(new ParameterImpl()));
		assertEquals(1, cl.get().getParameters().size());
	}

	@Test
	@SuppressWarnings("static-method")
	void fromWithNonEmptyParametersCopiesParametersViaLambda() {
		ContentLink src = new ContentLink();
		src.getParameters().add(new ParameterImpl());
		FluentContentLink result = new FluentContentLink().from(src);
		assertEquals(1, result.get().getParameters().size());
	}
}

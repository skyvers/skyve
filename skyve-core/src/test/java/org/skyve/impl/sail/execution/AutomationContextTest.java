package org.skyve.impl.sail.execution;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.web.UserAgentType;

class AutomationContextTest {

	/** A minimal concrete subclass for testing purposes. */
	private static AutomationContext<GenerateListContext, GenerateEditContext> newContext() {
		return new AutomationContext<>() {
			@Override public void generate(GenerateListContext lc) { /* no-op */ }
			@Override public void generate(GenerateEditContext ec) { /* no-op */ }
		};
	}

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorFieldsAreNull() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext();
		assertNull(ctx.getModuleName());
		assertNull(ctx.getDocumentName());
		assertNull(ctx.getViewType());
		assertNull(ctx.getUxui());
		assertNull(ctx.getUserAgentType());
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetModuleName() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext();
		ctx.setModuleName("admin");
		assertThat(ctx.getModuleName(), is("admin"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetDocumentName() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext();
		ctx.setDocumentName("Contact");
		assertThat(ctx.getDocumentName(), is("Contact"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetViewType() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext();
		ctx.setViewType(ViewType.edit);
		assertThat(ctx.getViewType(), is(ViewType.edit));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetUxui() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext();
		ctx.setUxui("desktop");
		assertThat(ctx.getUxui(), is("desktop"));
	}

	@Test
	@SuppressWarnings("static-method")
	void setAndGetUserAgentType() {
		AutomationContext<GenerateListContext, GenerateEditContext> ctx = newContext();
		ctx.setUserAgentType(UserAgentType.desktop);
		assertThat(ctx.getUserAgentType(), is(UserAgentType.desktop));
	}

	@Test
	@SuppressWarnings("static-method")
	void copyConstructorCopiesAllFields() {
		AutomationContext<GenerateListContext, GenerateEditContext> source = newContext();
		source.setModuleName("admin");
		source.setDocumentName("Contact");
		source.setViewType(ViewType.list);
		source.setUxui("desktop");
		source.setUserAgentType(UserAgentType.desktop);

		AutomationContext<GenerateListContext, GenerateEditContext> copy = new AutomationContext<>(source) {
			@Override public void generate(GenerateListContext lc) { /* no-op */ }
			@Override public void generate(GenerateEditContext ec) { /* no-op */ }
		};

		assertThat(copy.getModuleName(), is("admin"));
		assertThat(copy.getDocumentName(), is("Contact"));
		assertThat(copy.getViewType(), is(ViewType.list));
		assertThat(copy.getUxui(), is("desktop"));
		assertThat(copy.getUserAgentType(), is(UserAgentType.desktop));
	}

	@Test
	@SuppressWarnings("static-method")
	void copyConstructorWithNullFieldsPreservesNulls() {
		AutomationContext<GenerateListContext, GenerateEditContext> source = newContext();

		AutomationContext<GenerateListContext, GenerateEditContext> copy = new AutomationContext<>(source) {
			@Override public void generate(GenerateListContext lc) { /* no-op */ }
			@Override public void generate(GenerateEditContext ec) { /* no-op */ }
		};

		assertThat(copy.getModuleName(), is(nullValue()));
		assertThat(copy.getDocumentName(), is(nullValue()));
		assertThat(copy.getViewType(), is(nullValue()));
		assertThat(copy.getUxui(), is(nullValue()));
		assertThat(copy.getUserAgentType(), is(nullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void userAgentTypeIsMobileReturnsTrueForPhoneAndTablet() {
		assertTrue(UserAgentType.phone.isMobile());
		assertTrue(UserAgentType.tablet.isMobile());
		assertFalse(UserAgentType.desktop.isMobile());
	}
}

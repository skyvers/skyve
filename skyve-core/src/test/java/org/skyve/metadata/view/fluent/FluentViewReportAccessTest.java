package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.access.ViewReportUserAccessMetaData;
import org.skyve.metadata.module.ModuleMemberTestSupport.TestDocument;

/**
 * Tests for {@link FluentViewReportAccess}: constructors and setters.
 */
@SuppressWarnings("static-method")
class FluentViewReportAccessTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentViewReportAccess().get());
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		ViewReportUserAccessMetaData md = new ViewReportUserAccessMetaData();
		FluentViewReportAccess fa = new FluentViewReportAccess(md);
		assertSame(md, fa.get());
	}

	@Test
	void moduleNameSetsValue() {
		FluentViewReportAccess fa = new FluentViewReportAccess().moduleName("admin");
		assertEquals("admin", fa.get().getModuleName());
	}

	@Test
	void documentNameSetsValue() {
		FluentViewReportAccess fa = new FluentViewReportAccess().documentName("Contact");
		assertEquals("Contact", fa.get().getDocumentName());
	}

	@Test
	void reportNameSetsValue() {
		FluentViewReportAccess fa = new FluentViewReportAccess().reportName("ContactReport");
		assertEquals("ContactReport", fa.get().getReportName());
	}

	@Test
	void fromSetsAllFields() {
		FluentViewReportAccess fa = new FluentViewReportAccess()
				.from("admin", "Contact", "ContactReport", new HashSet<>());
		assertEquals("admin", fa.get().getModuleName());
		assertEquals("Contact", fa.get().getDocumentName());
		assertEquals("ContactReport", fa.get().getReportName());
	}

	@Test
	void getReturnsCorrectType() {
		FluentViewReportAccess fa = new FluentViewReportAccess();
		assertNotNull(fa.get());
	}

	@Test
	void typedFromSetsAllFields() {
		FluentViewReportAccess fa = new FluentViewReportAccess();
		FluentViewReportAccess result = fa.from(TestDocument.CONTACT, "rContacts", Set.of("desktop"));
		assertAll(() -> assertSame(fa, result),
					() -> assertEquals("admin", fa.get().getModuleName()),
					() -> assertEquals("Contact", fa.get().getDocumentName()),
					() -> assertEquals("rContacts", fa.get().getReportName()),
					() -> assertEquals(1, fa.get().getUxuis().size()));
	}
}

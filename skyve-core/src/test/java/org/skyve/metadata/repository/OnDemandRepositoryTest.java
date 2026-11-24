package org.skyve.metadata.repository;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.withSettings;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.impl.metadata.repository.behaviour.BizletMetaData;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.metadata.module.ModuleMemberTestSupport.TestDocument;

/**
 * Tests that the typed-reference default methods of {@link OnDemandRepository} delegate to the
 * String forms with the reference's module name and document name.
 */
@SuppressWarnings("static-method")
class OnDemandRepositoryTest {
	/** Lets Mockito's subclass mock maker call the interface's default methods for real. */
	private abstract static class OnDemandRepositoryDouble implements OnDemandRepository {
		// no state
	}

	@Test
	void typedLoadsDelegateToTheStringForms() {
		OnDemandRepository r = mock(OnDemandRepositoryDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		DocumentMetaData document = mock(DocumentMetaData.class);
		ViewMetaData view = mock(ViewMetaData.class);
		ActionMetaData action = mock(ActionMetaData.class);
		BizletMetaData bizlet = mock(BizletMetaData.class);
		doReturn(document).when(r).loadDocument("acme", "admin", "Contact");
		doReturn(view).when(r).loadView("acme", "admin", "Contact", "desktop", "edit");
		doReturn(action).when(r).loadMetaDataAction("acme", "admin", "Contact", "Save");
		doReturn(bizlet).when(r).loadMetaDataBizlet("acme", "admin", "Contact");

		assertAll(() -> assertSame(document, r.loadDocument("acme", TestDocument.CONTACT)),
					() -> assertSame(view, r.loadView("acme", TestDocument.CONTACT, "desktop", "edit")),
					() -> assertSame(action, r.loadMetaDataAction("acme", TestDocument.CONTACT, "Save")),
					() -> assertSame(bizlet, r.loadMetaDataBizlet("acme", TestDocument.CONTACT)));
	}

	@Test
	void typedLastModifiedMillisDelegateToTheStringForms() {
		OnDemandRepository r = mock(OnDemandRepositoryDouble.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		doReturn(Long.valueOf(1L)).when(r).documentLastModifiedMillis("acme", "admin", "Contact");
		doReturn(Long.valueOf(2L)).when(r).viewLastModifiedMillis("acme", "admin", "Contact", "desktop", "edit");
		doReturn(Long.valueOf(3L)).when(r).metaDataActionLastModifiedMillis("acme", "admin", "Contact", "Save");
		doReturn(Long.valueOf(4L)).when(r).metaDataBizletLastModifiedMillis("acme", "admin", "Contact");

		assertAll(() -> assertEquals(1L, r.documentLastModifiedMillis("acme", TestDocument.CONTACT)),
					() -> assertEquals(2L, r.viewLastModifiedMillis("acme", TestDocument.CONTACT, "desktop", "edit")),
					() -> assertEquals(3L, r.metaDataActionLastModifiedMillis("acme", TestDocument.CONTACT, "Save")),
					() -> assertEquals(4L, r.metaDataBizletLastModifiedMillis("acme", TestDocument.CONTACT)));
	}
}

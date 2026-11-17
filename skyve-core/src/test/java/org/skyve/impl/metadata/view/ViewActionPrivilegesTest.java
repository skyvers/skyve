package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;

/**
 * Tests {@link ViewActionPrivileges}: which privilege each kind of action needs before a view shows it.
 */
@SuppressWarnings("static-method")
class ViewActionPrivilegesTest {
	private final Document document = new DocumentImpl();

	@Test
	void customAndClassBasedActionsNeedTheirClassToBeExecutable() {
		User user = mock(User.class);
		when(Boolean.valueOf(user.canExecuteAction(document, "Approve"))).thenReturn(Boolean.TRUE);
		ActionImpl approve = action("Approve");
		ActionImpl export = action("Export");

		assertAll(() -> assertTrue(ViewActionPrivileges.permitted(user, document, null, approve)),
					() -> assertFalse(ViewActionPrivileges.permitted(user, document, null, export)),
					() -> assertFalse(ViewActionPrivileges.permitted(user, document, ImplicitActionName.BizExport, export)),
					() -> assertFalse(ViewActionPrivileges.permitted(user, document, ImplicitActionName.BizImport, export)),
					() -> assertFalse(ViewActionPrivileges.permitted(user, document, ImplicitActionName.Download, export)),
					() -> assertFalse(ViewActionPrivileges.permitted(user, document, ImplicitActionName.Upload, export)),
					() -> assertTrue(ViewActionPrivileges.permitted(user, document, ImplicitActionName.Download, approve)));
	}

	@Test
	void builtInActionsNeedTheirDocumentPrivilege() {
		ActionImpl action = new ActionImpl();
		User none = mock(User.class);
		User creator = mock(User.class);
		when(Boolean.valueOf(creator.canCreateDocument(document))).thenReturn(Boolean.TRUE);
		User updater = mock(User.class);
		when(Boolean.valueOf(updater.canUpdateDocument(document))).thenReturn(Boolean.TRUE);
		User deleter = mock(User.class);
		when(Boolean.valueOf(deleter.canDeleteDocument(document))).thenReturn(Boolean.TRUE);
		User reader = mock(User.class);
		when(Boolean.valueOf(reader.canReadDocument(document))).thenReturn(Boolean.TRUE);

		assertAll(() -> assertFalse(ViewActionPrivileges.permitted(none, document, ImplicitActionName.Add, action)),
					() -> assertTrue(ViewActionPrivileges.permitted(creator, document, ImplicitActionName.Add, action)),
					() -> assertTrue(ViewActionPrivileges.permitted(creator, document, ImplicitActionName.New, action)),
					() -> assertFalse(ViewActionPrivileges.permitted(none, document, ImplicitActionName.Delete, action)),
					() -> assertTrue(ViewActionPrivileges.permitted(deleter, document, ImplicitActionName.Delete, action)),
					() -> assertTrue(ViewActionPrivileges.permitted(deleter, document, ImplicitActionName.Remove, action)),
					() -> assertFalse(ViewActionPrivileges.permitted(none, document, ImplicitActionName.Edit, action)),
					() -> assertTrue(ViewActionPrivileges.permitted(reader, document, ImplicitActionName.Edit, action)),
					() -> assertFalse(ViewActionPrivileges.permitted(none, document, ImplicitActionName.Save, action)),
					() -> assertTrue(ViewActionPrivileges.permitted(creator, document, ImplicitActionName.Save, action)),
					() -> assertTrue(ViewActionPrivileges.permitted(updater, document, ImplicitActionName.Save, action)),
					() -> assertTrue(ViewActionPrivileges.permitted(updater, document, ImplicitActionName.OK, action)),
					() -> assertFalse(ViewActionPrivileges.permitted(deleter, document, ImplicitActionName.OK, action)),
					() -> assertTrue(ViewActionPrivileges.permitted(none, document, ImplicitActionName.Cancel, action)),
					() -> assertTrue(ViewActionPrivileges.permitted(none, document, ImplicitActionName.ZoomOut, action)));
	}

	@Test
	void aCustomActionWithoutAClassIsAMetaDataError() {
		ActionImpl action = new ActionImpl();
		action.setName("Nameless");
		User user = mock(User.class);

		MetaDataException e = assertThrows(MetaDataException.class, () -> ViewActionPrivileges.permitted(user, document, null, action));
		assertEquals("Action Nameless has no class name", e.getMessage());
	}

	private static ActionImpl action(String className) {
		ActionImpl result = new ActionImpl();
		result.setName(className);
		result.setResourceName(className);
		return result;
	}
}

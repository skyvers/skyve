package org.skyve.impl.metadata.view;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;

/**
 * Tests {@link ViewActionPrivileges} as a table: the privilege each action needs before a view shows it, checked for
 * every built-in action and a custom action against a user with each single privilege and one with none.
 */
class ViewActionPrivilegesTest {
	/** A privilege a user can have. */
	private enum Privilege {
		/** Execute the action's class. */
		EXECUTE,
		/** Create the document. */
		CREATE,
		/** Read the document. */
		READ,
		/** Update the document. */
		UPDATE,
		/** Delete the document. */
		DELETE
	}

	/** The privileges each built-in action accepts (any one is enough; none means it needs none). */
	private static final Map<ImplicitActionName, Set<Privilege>> NEEDS = new EnumMap<>(ImplicitActionName.class);
	static {
		Set<Privilege> none = EnumSet.noneOf(Privilege.class);
		NEEDS.put(ImplicitActionName.DEFAULTS, none);
		NEEDS.put(ImplicitActionName.OK, EnumSet.of(Privilege.CREATE, Privilege.UPDATE));
		NEEDS.put(ImplicitActionName.Save, EnumSet.of(Privilege.CREATE, Privilege.UPDATE));
		NEEDS.put(ImplicitActionName.Delete, EnumSet.of(Privilege.DELETE));
		NEEDS.put(ImplicitActionName.Add, EnumSet.of(Privilege.CREATE));
		NEEDS.put(ImplicitActionName.ZoomOut, none);
		NEEDS.put(ImplicitActionName.Cancel, none);
		NEEDS.put(ImplicitActionName.Remove, EnumSet.of(Privilege.DELETE));
		NEEDS.put(ImplicitActionName.New, EnumSet.of(Privilege.CREATE));
		NEEDS.put(ImplicitActionName.Edit, EnumSet.of(Privilege.READ));
		NEEDS.put(ImplicitActionName.Report, none);
		NEEDS.put(ImplicitActionName.Navigate, none);
		NEEDS.put(ImplicitActionName.BizImport, EnumSet.of(Privilege.EXECUTE));
		NEEDS.put(ImplicitActionName.BizExport, EnumSet.of(Privilege.EXECUTE));
		NEEDS.put(ImplicitActionName.Download, EnumSet.of(Privilege.EXECUTE));
		NEEDS.put(ImplicitActionName.Upload, EnumSet.of(Privilege.EXECUTE));
		NEEDS.put(ImplicitActionName.Print, none);
	}

	/** The class (resource) name of the action under test. */
	private static final String CLASS_NAME = "Approve";

	private final Document document = new DocumentImpl();

	@Test
	@SuppressWarnings("static-method")
	void everyBuiltInActionIsInTheTable() {
		assertEquals(EnumSet.allOf(ImplicitActionName.class), NEEDS.keySet());
	}

	/**
	 * Each built-in action, and a custom one (which needs its class executable), is shown to a user with a privilege
	 * it accepts and to no one with another; one needing none is shown even to a user with no privileges.
	 */
	@Test
	void eachActionNeedsOneOfItsPrivileges() {
		ActionImpl action = new ActionImpl();
		action.setName(CLASS_NAME);
		action.setResourceName(CLASS_NAME);
		List<Executable> checks = new ArrayList<>();
		for (Privilege privilege : Privilege.values()) {
			User user = userWith(privilege);
			checks.add(() -> assertEquals(Boolean.valueOf(privilege == Privilege.EXECUTE),
											Boolean.valueOf(ViewActionPrivileges.permitted(user, document, null, action)),
											"custom action for a user who can " + privilege));
			for (ImplicitActionName name : ImplicitActionName.values()) {
				Set<Privilege> needs = NEEDS.get(name);
				Boolean expected = Boolean.valueOf(needs.isEmpty() || needs.contains(privilege));
				checks.add(() -> assertEquals(expected,
												Boolean.valueOf(ViewActionPrivileges.permitted(user, document, name, action)),
												name + " for a user who can " + privilege));
			}
		}
		User nobody = mock(User.class);
		for (ImplicitActionName name : ImplicitActionName.values()) {
			Boolean expected = Boolean.valueOf(NEEDS.get(name).isEmpty());
			checks.add(() -> assertEquals(expected,
											Boolean.valueOf(ViewActionPrivileges.permitted(nobody, document, name, action)),
											name + " for a user with no privileges"));
		}
		assertAll(checks);
	}

	/**
	 * A custom or class-based action without a class is a metadata error; a built-in action that needs no class does
	 * not need one.
	 */
	@Test
	void anActionThatNeedsAClassButHasNoneIsAMetaDataError() {
		ActionImpl classless = new ActionImpl();
		classless.setName("Nameless");
		User user = userWith(Privilege.EXECUTE);
		List<Executable> checks = new ArrayList<>();
		for (ImplicitActionName name : new ImplicitActionName[] {null,
																	ImplicitActionName.BizExport,
																	ImplicitActionName.BizImport,
																	ImplicitActionName.Download,
																	ImplicitActionName.Upload}) {
			checks.add(() -> assertEquals("Action Nameless has no class name",
											assertThrows(MetaDataException.class,
															() -> ViewActionPrivileges.permitted(user, document, name, classless),
															String.valueOf(name)).getMessage()));
		}
		checks.add(() -> assertEquals(Boolean.TRUE,
										Boolean.valueOf(ViewActionPrivileges.permitted(user, document, ImplicitActionName.Cancel, classless))));
		assertAll(checks);
	}

	/**
	 * Returns a user with one privilege on the test document only.
	 *
	 * @param privilege the privilege
	 * @return the user
	 */
	private User userWith(Privilege privilege) {
		User result = mock(User.class);
		Boolean granted = switch (privilege) {
			case EXECUTE -> Boolean.valueOf(result.canExecuteAction(document, CLASS_NAME));
			case CREATE -> Boolean.valueOf(result.canCreateDocument(document));
			case READ -> Boolean.valueOf(result.canReadDocument(document));
			case UPDATE -> Boolean.valueOf(result.canUpdateDocument(document));
			case DELETE -> Boolean.valueOf(result.canDeleteDocument(document));
		};
		when(granted).thenReturn(Boolean.TRUE);
		return result;
	}
}

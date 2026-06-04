package modules.admin.UserDashboard;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Timestamp;

import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;
import modules.admin.domain.Content;
import modules.admin.domain.User;
import util.AbstractH2Test;

/**
 * Tests dashboard tile creation branches using real metadata permissions.
 */
@SuppressWarnings("static-method")
class UserDashboardExtensionTest extends AbstractH2Test {
	@Test
	void createTileReturnsNullForUnknownDocument() throws Exception {
		assertNull(createTile(Operation.update, "missing", "Missing", null, "Unknown"));
	}

	@Test
	void createTileForInsertUsesCreateActionTextAndIcon() throws Exception {
		Tile tile = createTile(Operation.insert, User.MODULE_NAME, User.DOCUMENT_NAME, null, "Suggested for creation");

		assertNotNull(tile);
		assertThat(tile.getOperation(), is(Tile.Operation.insert.toString()));
		assertThat(tile.getAction(), is("Create a new "));
		assertThat(tile.getActionClass(), is("fa-plus"));
		assertThat(tile.toMarkup(), containsString("Suggested for creation"));
	}

	@Test
	void createTileForUpdateUsesPersistentUpdateAction() throws Exception {
		Tile tile = createTile(Operation.update, User.MODULE_NAME, User.DOCUMENT_NAME, null, "Recent by me");

		assertNotNull(tile);
		assertThat(tile.getOperation(), is(Tile.Operation.update.toString()));
		assertThat(tile.getActionClass(), is("fa-angle-up"));
		assertThat(tile.toMarkup(), containsString("Recent by me"));
	}

	@Test
	void createTileForDeleteUsesDeleteAction() throws Exception {
		Tile tile = createTile(Operation.delete, User.MODULE_NAME, User.DOCUMENT_NAME, null, "Delete test");

		assertNotNull(tile);
		assertThat(tile.getOperation(), is(Tile.Operation.delete.toString()));
		assertThat(tile.getAction(), is("Delete "));
		assertThat(tile.getActionClass(), is("fa-times"));
	}

	@Test
	void createTileForNonPersistentUpdateUsesViewAction() throws Exception {
		Tile tile = createTile(Operation.update, Content.MODULE_NAME, Content.DOCUMENT_NAME, null, "View content");

		assertNotNull(tile);
		assertThat(tile.getOperation(), is(Tile.Operation.view.toString()));
		assertThat(tile.getAction(), is("View "));
		assertThat(tile.getActionClass(), is("fa-chevron-right"));
	}

	@Test
	void createTileWithContentBeanAppendsBizKeyToTitle() throws Exception {
		Content content = Content.newInstance();
		content.setContent("content-id-123");

		Tile tile = createTile(Operation.update, Content.MODULE_NAME, Content.DOCUMENT_NAME, content, "Recent content");

		assertNotNull(tile);
		assertThat(tile.getTitle(), containsString("View  Content - modules.admin.domain.Content@"));
	}

	@Test
	void addTileRejectsNullDuplicateAndLimitOverflow() throws Exception {
		UserDashboardExtension dashboard = new UserDashboardExtension();
		Tile first = new Tile.Builder().link("first").title("First").build();
		Tile duplicate = new Tile.Builder().link("first").title("Duplicate").build();

		assertFalse(addTile(dashboard, null));
		assertTrue(addTile(dashboard, first));
		assertFalse(addTile(dashboard, duplicate));
		assertTrue(addTile(dashboard, new Tile.Builder().link("second").build()));
		assertTrue(addTile(dashboard, new Tile.Builder().link("third").build()));
		assertTrue(addTile(dashboard, new Tile.Builder().link("fourth").build()));
		assertTrue(addTile(dashboard, new Tile.Builder().link("fifth").build()));
		assertTrue(addTile(dashboard, new Tile.Builder().link("sixth").build()));
		assertFalse(addTile(dashboard, new Tile.Builder().link("seventh").build()));
		assertEquals(6, tileSet(dashboard).size());
	}

	@Test
	void checkModuleDocumentCanBeReadReturnsTrueForKnownReadableDocument() throws Exception {
		assertTrue(checkModuleDocumentCanBeRead(User.MODULE_NAME, User.DOCUMENT_NAME));
	}

	@Test
	void checkModuleDocumentCanBeReadReturnsFalseForMissingModuleOrDocument() throws Exception {
		assertFalse(checkModuleDocumentCanBeRead("missing", User.DOCUMENT_NAME));
		assertFalse(checkModuleDocumentCanBeRead(User.MODULE_NAME, "Missing"));
	}

	@Test
	void createTilesRecentForInsertAddsReadableTileAndSkipsMissingDocument() throws Exception {
		UserDashboardExtension dashboard = new UserDashboardExtension();
		List<org.skyve.domain.Bean> audits = List.of(
				auditProjection(100L, "missing", "Missing"),
				auditProjection(200L, User.MODULE_NAME, User.DOCUMENT_NAME));

		createTilesRecent(dashboard, audits, Operation.insert, 2, "Recently created");

		Set<Tile> tiles = tileSet(dashboard);
		assertEquals(1, tiles.size());
		assertTrue(tiles.stream().allMatch(tile -> Tile.Operation.insert.toString().equals(tile.getOperation())));
		assertTrue(tiles.stream().allMatch(tile -> tile.toMarkup().contains("Recently created")));
		assertTrue(tiles.stream().anyMatch(tile -> tile.getTitle().contains("User")));
	}

	private static Tile createTile(Operation operation, String moduleName, String documentName, org.skyve.domain.Bean bean,
			String reason) throws Exception {
		Method method = UserDashboardExtension.class.getDeclaredMethod("createTile",
				Operation.class,
				String.class,
				String.class,
				org.skyve.domain.Bean.class,
				String.class);
		method.setAccessible(true);
		try {
			return (Tile) method.invoke(null, operation, moduleName, documentName, bean, reason);
		} catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	private static boolean addTile(UserDashboardExtension dashboard, Tile tile) throws Exception {
		Method method = UserDashboardExtension.class.getDeclaredMethod("addTile", Tile.class);
		method.setAccessible(true);
		return ((Boolean) method.invoke(dashboard, tile)).booleanValue();
	}

	private static void createTilesRecent(UserDashboardExtension dashboard, List<org.skyve.domain.Bean> audits,
			Operation operation, int top, String reason) throws Exception {
		Method method = UserDashboardExtension.class.getDeclaredMethod("createTilesRecent",
				List.class,
				Operation.class,
				int.class,
				String.class);
		method.setAccessible(true);
		try {
			method.invoke(dashboard, audits, operation, Integer.valueOf(top), reason);
		} catch (InvocationTargetException e) {
			Throwable cause = e.getCause();
			if (cause instanceof Exception exception) {
				throw exception;
			}
			throw e;
		}
	}

	@SuppressWarnings("unchecked")
	private static Set<Tile> tileSet(UserDashboardExtension dashboard) throws Exception {
		Field field = UserDashboardExtension.class.getDeclaredField("tiles");
		field.setAccessible(true);
		return (Set<Tile>) field.get(dashboard);
	}

	private static Audit auditProjection(long timestamp, String moduleName, String documentName) {
		Audit audit = Audit.newInstance();
		audit.setTimestamp(new Timestamp(timestamp));
		audit.setAuditModuleName(moduleName);
		audit.setAuditDocumentName(documentName);
		return audit;
	}

	private static boolean checkModuleDocumentCanBeRead(String moduleName, String documentName) throws Exception {
		Method method = UserDashboardExtension.class.getDeclaredMethod("checkModuleDocumentCanBeRead", String.class, String.class);
		method.setAccessible(true);
		return ((Boolean) method.invoke(null, moduleName, documentName)).booleanValue();
	}
}

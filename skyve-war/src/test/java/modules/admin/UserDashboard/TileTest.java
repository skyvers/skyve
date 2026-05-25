package modules.admin.UserDashboard;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

/**
 * Tests for the Tile POJO and its Builder.
 */
class TileTest {

	// ======== Builder tests ========

	@SuppressWarnings("static-method")
	@Test
	void testBuilderCreatesNonNullTile() {
		Tile tile = new Tile.Builder().build();
		assertNotNull(tile);
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderSetsAction() {
		Tile tile = new Tile.Builder().action("View").build();
		assertThat(tile.getAction(), is("View"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderSetsActionClass() {
		Tile tile = new Tile.Builder().actionClass("fa-eye").build();
		assertThat(tile.getActionClass(), is("fa-eye"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderSetsIcon() {
		Tile tile = new Tile.Builder().icon("<i class='fa-file'/>").build();
		assertThat(tile.getIcon(), is("<i class='fa-file'/>"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderSetsNonEmptyLinkAsOnclick() {
		Tile tile = new Tile.Builder().link("http://example.com/page").build();
		String link = tile.getLink();
		assertThat(link, containsString("onclick"));
		assertThat(link, containsString("http://example.com/page"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderSetsNullLinkAsNull() {
		Tile tile = new Tile.Builder().link(null).build();
		assertNull(tile.getLink());
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderSetsEmptyLinkAsNull() {
		Tile tile = new Tile.Builder().link("").build();
		assertNull(tile.getLink());
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderSetsReason() {
		Tile tile = new Tile.Builder().reason("Popular by me").build();
		assertThat(tile.getReason(), is("Popular by me"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderSetsTitle() {
		Tile tile = new Tile.Builder().title("Short title").build();
		assertThat(tile.getTitle(), is("Short title"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderSetsInsertOperation() {
		Tile tile = new Tile.Builder().operation(Tile.Operation.insert).build();
		assertThat(tile.getOperation(), is("insert"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderSetsUpdateOperation() {
		Tile tile = new Tile.Builder().operation(Tile.Operation.update).build();
		assertThat(tile.getOperation(), is("update"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderSetsDeleteOperation() {
		Tile tile = new Tile.Builder().operation(Tile.Operation.delete).build();
		assertThat(tile.getOperation(), is("delete"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderSetsViewOperation() {
		Tile tile = new Tile.Builder().operation(Tile.Operation.view).build();
		assertThat(tile.getOperation(), is("view"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testBuilderChainsCorrectly() {
		Tile tile = new Tile.Builder()
				.action("Edit")
				.actionClass("fa-edit")
				.icon("<i class='fa-pencil'/>")
				.link("http://example.com/edit")
				.operation(Tile.Operation.update)
				.reason("Recent by me")
				.title("My Document")
				.build();

		assertThat(tile.getAction(), is("Edit"));
		assertThat(tile.getActionClass(), is("fa-edit"));
		assertThat(tile.getIcon(), is("<i class='fa-pencil'/>"));
		assertThat(tile.getLink(), containsString("http://example.com/edit"));
		assertThat(tile.getOperation(), is("update"));
		assertThat(tile.getReason(), is("Recent by me"));
		assertThat(tile.getTitle(), is("My Document"));
	}

	// ======== Title truncation tests ========

	@SuppressWarnings("static-method")
	@Test
	void testShortTitleIsNotTruncated() {
		String shortTitle = "Short Title"; // less than 50 chars
		Tile tile = new Tile.Builder().title(shortTitle).build();
		assertThat(tile.getTitle(), is(shortTitle));
	}

	@SuppressWarnings("static-method")
	@Test
	void testTitleExactly50CharsIsNotTruncated() {
		String title = "A".repeat(50); // exactly 50 chars
		Tile tile = new Tile.Builder().title(title).build();
		assertThat(tile.getTitle(), is(title));
	}

	@SuppressWarnings("static-method")
	@Test
	void testTitleOver50CharsIsTruncated() {
		String longTitle = "A".repeat(60); // more than 50 chars
		Tile tile = new Tile.Builder().title(longTitle).build();
		String result = tile.getTitle();
		assertNotNull(result);
		assertTrue(result.length() <= 50);
		assertThat(result, containsString("..."));
	}

	@SuppressWarnings("static-method")
	@Test
	void testTruncatedTitleEndsWith3Dots() {
		String longTitle = "This is a very long title that exceeds fifty characters in length";
		Tile tile = new Tile.Builder().title(longTitle).build();
		assertTrue(tile.getTitle().endsWith("..."));
	}

	@SuppressWarnings("static-method")
	@Test
	void testNullTitleReturnsNull() {
		Tile tile = new Tile.Builder().title(null).build();
		assertNull(tile.getTitle());
	}

	// ======== Operation enum tests ========

	@SuppressWarnings("static-method")
	@Test
	void testOperationEnumValues() {
		Tile.Operation[] operations = Tile.Operation.values();
		assertEquals(4, operations.length);
	}

	@SuppressWarnings("static-method")
	@Test
	void testOperationInsert() {
		Tile.Operation op = Tile.Operation.insert;
		assertThat(op.toString(), is("insert"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testOperationUpdate() {
		Tile.Operation op = Tile.Operation.update;
		assertThat(op.toString(), is("update"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testOperationDelete() {
		Tile.Operation op = Tile.Operation.delete;
		assertThat(op.toString(), is("delete"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testOperationView() {
		Tile.Operation op = Tile.Operation.view;
		assertThat(op.toString(), is("view"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testOperationValueOf() {
		assertThat(Tile.Operation.valueOf("insert"), is(Tile.Operation.insert));
		assertThat(Tile.Operation.valueOf("update"), is(Tile.Operation.update));
		assertThat(Tile.Operation.valueOf("delete"), is(Tile.Operation.delete));
		assertThat(Tile.Operation.valueOf("view"), is(Tile.Operation.view));
	}

	// ======== equals and hashCode tests ========

	@SuppressWarnings("static-method")
	@Test
	void testTilesWithSameLinkAreEqual() {
		Tile tile1 = new Tile.Builder().link("http://example.com/page").title("Title 1").build();
		Tile tile2 = new Tile.Builder().link("http://example.com/page").title("Title 2").build();
		assertEquals(tile1, tile2);
	}

	@SuppressWarnings("static-method")
	@Test
	void testTilesWithDifferentLinksAreNotEqual() {
		Tile tile1 = new Tile.Builder().link("http://example.com/page1").build();
		Tile tile2 = new Tile.Builder().link("http://example.com/page2").build();
		assertNotEquals(tile1, tile2);
	}

	@SuppressWarnings("static-method")
	@Test
	void testTileEqualsItself() {
		Tile tile = new Tile.Builder().link("http://example.com/page").build();
		assertEquals(tile, tile);
	}

	@SuppressWarnings("static-method")
	@Test
	void testTileNotEqualsNull() {
		Tile tile = new Tile.Builder().link("http://example.com/page").build();
		assertNotEquals(null, tile);
	}

	@SuppressWarnings("static-method")
	@Test
	void testTileNotEqualsDifferentType() {
		Tile tile = new Tile.Builder().link("http://example.com/page").build();
		assertNotEquals("someString", tile);
	}

	@SuppressWarnings("static-method")
	@Test
	void testTileWithNullLinkEqualsAnotherNullLinkTile() {
		Tile tile1 = new Tile.Builder().title("Title 1").build();
		Tile tile2 = new Tile.Builder().title("Title 2").build();
		assertEquals(tile1, tile2);
	}

	@SuppressWarnings("static-method")
	@Test
	void testTileWithLinkNotEqualsTileWithNullLink() {
		Tile tile1 = new Tile.Builder().link("http://example.com/page").build();
		Tile tile2 = new Tile.Builder().title("Title").build();
		assertNotEquals(tile1, tile2);
	}

	@SuppressWarnings("static-method")
	@Test
	void testTilesWithSameLinkHaveSameHashCode() {
		Tile tile1 = new Tile.Builder().link("http://example.com/page").title("A").build();
		Tile tile2 = new Tile.Builder().link("http://example.com/page").title("B").build();
		assertEquals(tile1.hashCode(), tile2.hashCode());
	}

	@SuppressWarnings("static-method")
	@Test
	void testTilesWithNullLinkHaveSameHashCode() {
		Tile tile1 = new Tile.Builder().title("Title 1").build();
		Tile tile2 = new Tile.Builder().title("Title 2").build();
		assertEquals(tile1.hashCode(), tile2.hashCode());
	}

	// ======== toMarkup tests ========

	@SuppressWarnings("static-method")
	@Test
	void testToMarkupContainsOperation() {
		Tile tile = new Tile.Builder()
				.operation(Tile.Operation.insert)
				.icon("<i/>")
				.actionClass("fa-plus")
				.title("Create a document")
				.reason("Recently created")
				.build();
		String markup = tile.toMarkup();
		assertThat(markup, containsString("insert"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testToMarkupContainsIcon() {
		Tile tile = new Tile.Builder()
				.operation(Tile.Operation.view)
				.icon("<span class='icon'><i class='fa-file'/></span>")
				.actionClass("fa-chevron-right")
				.title("Document Title")
				.reason("Popular")
				.build();
		String markup = tile.toMarkup();
		assertThat(markup, containsString("fa-file"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testToMarkupContainsActionClass() {
		Tile tile = new Tile.Builder()
				.operation(Tile.Operation.delete)
				.icon("<i/>")
				.actionClass("fa-times")
				.title("Delete document")
				.reason("Test")
				.build();
		String markup = tile.toMarkup();
		assertThat(markup, containsString("fa-times"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testToMarkupContainsTitleText() {
		Tile tile = new Tile.Builder()
				.operation(Tile.Operation.update)
				.icon("<i/>")
				.actionClass("fa-edit")
				.title("My Document Title")
				.reason("Recent by me")
				.build();
		String markup = tile.toMarkup();
		assertThat(markup, containsString("My Document Title"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testToMarkupContainsReason() {
		Tile tile = new Tile.Builder()
				.operation(Tile.Operation.update)
				.icon("<i/>")
				.actionClass("fa-edit")
				.title("Title")
				.reason("Popular by me")
				.build();
		String markup = tile.toMarkup();
		assertThat(markup, containsString("Popular by me"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testToMarkupContainsDivTag() {
		Tile tile = new Tile.Builder()
				.operation(Tile.Operation.view)
				.icon("<i/>")
				.actionClass("fa-eye")
				.title("Title")
				.reason("Reason")
				.build();
		String markup = tile.toMarkup();
		assertThat(markup, containsString("<div"));
		assertThat(markup, containsString("</div>"));
	}

	@SuppressWarnings("static-method")
	@Test
	void testToMarkupWithLinkContainsOnclick() {
		Tile tile = new Tile.Builder()
				.operation(Tile.Operation.update)
				.icon("<i/>")
				.actionClass("fa-edit")
				.link("http://example.com/edit")
				.title("Edit")
				.reason("Recent")
				.build();
		String markup = tile.toMarkup();
		assertThat(markup, containsString("onclick"));
	}

	// ======== TWO_WEEKS_AGO field test ========

	@SuppressWarnings("static-method")
	@Test
	void testTwoWeeksAgoIsInThePast() {
		assertTrue(UserDashboardExtension.TWO_WEEKS_AGO.longValue() < System.currentTimeMillis());
	}

	@SuppressWarnings("static-method")
	@Test
	void testTwoWeeksAgoIsApproximately14DaysAgo() {
		long twoWeeksInMs = 1209600000L;
		long now = System.currentTimeMillis();
		long diff = now - UserDashboardExtension.TWO_WEEKS_AGO.longValue();
		// should be within 1 minute of expected value
		assertTrue(Math.abs(diff - twoWeeksInMs) < 60000L);
	}
}

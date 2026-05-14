package org.skyve.metadata.module.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.menu.EditItem;
import org.skyve.impl.metadata.module.menu.ListItem;
import org.skyve.impl.metadata.module.menu.MenuGroupImpl;
import org.skyve.impl.metadata.repository.module.BizQLReferenceMetaData;
import org.skyve.impl.metadata.repository.module.EditItemMetaData;
import org.skyve.impl.metadata.repository.module.GroupMetaData;
import org.skyve.impl.metadata.repository.module.ListItemMetaData;
import org.skyve.impl.metadata.repository.module.MetaDataQueryReferenceMetaData;
import org.skyve.impl.metadata.repository.module.SQLReferenceMetaData;

/**
 * Tests for the smaller module fluent builder types not fully covered elsewhere:
 * FluentEditItem, FluentListItem, FluentSQLReference, FluentBizQLReference,
 * FluentMetaDataQueryReference.
 */
@SuppressWarnings("static-method")
class FluentModuleSmallBuildersTest {

	// ---- FluentEditItem -----------------------------------------------

	@Test
	void editItemWrappingConstructorPreservesInstance() {
		EditItemMetaData md = new EditItemMetaData();
		FluentEditItem fluent = new FluentEditItem(md);
		assertSame(md, fluent.get());
	}

	@Test
	void editItemDocumentNameSetsValue() {
		FluentEditItem fluent = new FluentEditItem().documentName("Contact");
		assertEquals("Contact", fluent.get().getDocumentName());
	}

	// ---- FluentListItem -----------------------------------------------

	@Test
	void listItemWrappingConstructorPreservesInstance() {
		ListItemMetaData md = new ListItemMetaData();
		FluentListItem fluent = new FluentListItem(md);
		assertSame(md, fluent.get());
	}

	@Test
	void listItemDocumentNameSetsValue() {
		FluentListItem fluent = new FluentListItem().documentName("Contact");
		assertEquals("Contact", fluent.get().getDocumentName());
	}

	@Test
	void listItemQueryNameSetsValue() {
		FluentListItem fluent = new FluentListItem().queryName("qContacts");
		assertEquals("qContacts", fluent.get().getQueryName());
	}

	@Test
	void listItemModelNameSetsValue() {
		FluentListItem fluent = new FluentListItem().modelName("ContactModel");
		assertEquals("ContactModel", fluent.get().getModelName());
	}

	@Test
	void listItemAutoPopulateSetsTrue() {
		FluentListItem fluent = new FluentListItem().autoPopulate(true);
		assertEquals(Boolean.TRUE, fluent.get().getAutoPopulate());
	}

	@Test
	void listItemDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentListItem().get());
	}

	// ---- FluentSQLReference -------------------------------------------

	@Test
	void sqlReferenceWrappingConstructorPreservesInstance() {
		SQLReferenceMetaData md = new SQLReferenceMetaData();
		FluentSQLReference fluent = new FluentSQLReference(md);
		assertSame(md, fluent.get());
	}

	@Test
	void sqlReferenceDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentSQLReference().get());
	}

	// ---- FluentBizQLReference -----------------------------------------

	@Test
	void bizqlReferenceWrappingConstructorPreservesInstance() {
		BizQLReferenceMetaData md = new BizQLReferenceMetaData();
		FluentBizQLReference fluent = new FluentBizQLReference(md);
		assertSame(md, fluent.get());
	}

	@Test
	void bizqlReferenceDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentBizQLReference().get());
	}

	// ---- FluentMetaDataQueryReference ---------------------------------

	@Test
	void metaDataQueryReferenceWrappingConstructorPreservesInstance() {
		MetaDataQueryReferenceMetaData md = new MetaDataQueryReferenceMetaData();
		FluentMetaDataQueryReference fluent = new FluentMetaDataQueryReference(md);
		assertSame(md, fluent.get());
	}

	@Test
	void metaDataQueryReferenceDefaultConstructorCreatesInstance() {
		assertNotNull(new FluentMetaDataQueryReference().get());
	}

	// ---- FluentMenuGroup wrapping constructor and from() ---------------

	@Test
	void menuGroupWrappingConstructorPreservesInstance() {
		GroupMetaData md = new GroupMetaData();
		FluentMenuGroup fluent = new FluentMenuGroup(md);
		assertSame(md, fluent.get());
	}

	@Test
	void menuGroupFromWithEditItemAddsToGroup() {
		MenuGroupImpl source = new MenuGroupImpl();
		EditItem editItem = new EditItem();
		editItem.setName("EditUser");
		editItem.setDocumentName("User");
		source.getItems().add(editItem);

		FluentMenuGroup fluent = new FluentMenuGroup().from(source);
		assertNotNull(fluent.findEditItem("EditUser"));
	}

	@Test
	void menuGroupFromWithListItemAddsToGroup() {
		MenuGroupImpl source = new MenuGroupImpl();
		ListItem listItem = new ListItem();
		listItem.setName("ListUsers");
		listItem.setDocumentName("User");
		source.getItems().add(listItem);

		FluentMenuGroup fluent = new FluentMenuGroup().from(source);
		assertNotNull(fluent.findListItem("ListUsers"));
	}
}

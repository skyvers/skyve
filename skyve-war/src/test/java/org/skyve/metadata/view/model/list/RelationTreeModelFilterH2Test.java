package org.skyve.metadata.view.model.list;

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.impl.util.UUIDv7;

import modules.admin.domain.Contact;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class RelationTreeModelFilterH2Test extends AbstractH2Test {
	@Test
	void filterAddsNoRowsWhenTheExpandedNodeIsNotInTheObjectGraph() {
		Contact contact = Contact.newInstance();
		RelationTreeModelFilter<Contact> filter = new RelationTreeModelFilter<>(contact);
		// A node ID is a bizId followed by a random suffix
		filter.addEquals(HierarchicalBean.PARENT_ID, UUIDv7.create().toString() + UUIDv7.create().toString());

		List<Bean> rows = new ArrayList<>();
		filter.filter(rows);

		assertTrue(rows.isEmpty());
	}
}

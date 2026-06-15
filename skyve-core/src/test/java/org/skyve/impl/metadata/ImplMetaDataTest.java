package org.skyve.impl.metadata;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.SortDirection;

@SuppressWarnings("static-method")
class ImplMetaDataTest {

	// ---- OrderingImpl ----

	@Test
	void orderingDefaultConstructorByIsNull() {
		OrderingImpl o = new OrderingImpl();
		assertThat(o.getBy(), is(nullValue()));
	}

	@Test
	void orderingDefaultConstructorSortIsNull() {
		OrderingImpl o = new OrderingImpl();
		assertThat(o.getSort(), is(nullValue()));
	}

	@Test
	void orderingParameterisedConstructor() {
		OrderingImpl o = new OrderingImpl("name", SortDirection.ascending);
		assertThat(o.getBy(), is("name"));
		assertThat(o.getSort(), is(SortDirection.ascending));
	}

	@Test
	void orderingSetBy() {
		OrderingImpl o = new OrderingImpl();
		o.setBy("createdDate");
		assertThat(o.getBy(), is("createdDate"));
	}

	@Test
	void orderingSetByTrimsBlankToNull() {
		OrderingImpl o = new OrderingImpl();
		o.setBy("   ");
		assertThat(o.getBy(), is(nullValue()));
	}

	@Test
	void orderingSetSort() {
		OrderingImpl o = new OrderingImpl();
		o.setSort(SortDirection.descending);
		assertThat(o.getSort(), is(SortDirection.descending));
	}

	@Test
	void orderingSetSortNull() {
		OrderingImpl o = new OrderingImpl("name", SortDirection.ascending);
		o.setSort(null);
		assertThat(o.getSort(), is(nullValue()));
	}

	// ---- AbstractMetaDataMap (via a concrete subclass) ----

	@Test
	void abstractMetaDataMapPutAndGetMetaData() {
		// Use ModuleImpl which extends AbstractMetaDataMap
		org.skyve.impl.metadata.module.ModuleImpl m = new org.skyve.impl.metadata.module.ModuleImpl();
		m.setName("M");
		// Verify getDocumentRefs not null (indirect test of the map)
		assertThat(m.getDocumentRefs(), is(notNullValue()));
	}

	@Test
	void abstractMetaDataMapPutDuplicateThrows() {
		org.skyve.impl.metadata.module.ModuleImpl m = new org.skyve.impl.metadata.module.ModuleImpl();
		m.setName("M");
		org.skyve.impl.metadata.module.query.BizQLDefinitionImpl q1 = new org.skyve.impl.metadata.module.query.BizQLDefinitionImpl();
		q1.setName("dup");
		q1.setDescription("d1");
		org.skyve.impl.metadata.module.query.BizQLDefinitionImpl q2 = new org.skyve.impl.metadata.module.query.BizQLDefinitionImpl();
		q2.setName("dup");
		q2.setDescription("d2");
		m.putQuery(q1);
		assertThrows(IllegalStateException.class, () -> m.putQuery(q2));
	}

	// ---- SortDirection (referenced by OrderingImpl) ----

	@Test
	void sortDirectionAscendingNotNull() {
		assertThat(SortDirection.ascending, is(notNullValue()));
	}

	@Test
	void sortDirectionDescendingNotNull() {
		assertThat(SortDirection.descending, is(notNullValue()));
	}
}

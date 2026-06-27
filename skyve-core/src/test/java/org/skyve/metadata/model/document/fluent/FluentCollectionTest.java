package org.skyve.metadata.model.document.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.CollectionImpl;
import org.skyve.metadata.model.document.Collection.CollectionType;

@SuppressWarnings("static-method")
class FluentCollectionTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertThat(new FluentCollection().get(), is(notNullValue()));
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		CollectionImpl impl = new CollectionImpl();
		assertThat(new FluentCollection(impl).get(), is(impl));
	}

	@Test
	void typeSetsValue() {
		assertThat(new FluentCollection().type(CollectionType.child).get().getType(), is(CollectionType.child));
	}

	@Test
	void orderedSetsTrue() {
		assertThat(new FluentCollection().ordered(true).get().getOrdered(), is(Boolean.TRUE));
	}

	@Test
	void orderedSetsFalse() {
		assertThat(new FluentCollection().ordered(false).get().getOrdered(), is(Boolean.FALSE));
	}

	@Test
	void minCardinalitySetsValue() {
		assertEquals(2, new FluentCollection().minCardinality(2).get().getMinCardinality());
	}

	@Test
	void maxCardinalitySetsValue() {
		assertThat(new FluentCollection().maxCardinality(10).get().getMaxCardinality(), is(Integer.valueOf(10)));
	}

	@Test
	void ownerDatabaseIndexSetsTrue() {
		assertThat(new FluentCollection().ownerDatabaseIndex(true).get().getOwnerDatabaseIndex(), is(Boolean.TRUE));
	}

	@Test
	void elementDatabaseIndexSetsTrue() {
		assertThat(new FluentCollection().elementDatabaseIndex(true).get().getElementDatabaseIndex(), is(Boolean.TRUE));
	}

	@Test
	void cacheNameSetsValue() {
		assertThat(new FluentCollection().cacheName("myCache").get().getCacheName(), is("myCache"));
	}

	@Test
	void addOrderingAddsEntry() {
		FluentCollection col = new FluentCollection().addOrdering(new FluentCollectionOrdering().by("name"));
		assertEquals(1, col.get().getOrdering().size());
	}

	@Test
	void removeOrderingRemovesEntry() {
		FluentCollection col = new FluentCollection()
				.addOrdering(new FluentCollectionOrdering().by("name"))
				.removeOrdering("name");
		assertEquals(0, col.get().getOrdering().size());
	}

	@Test
	void clearOrderingRemovesAll() {
		FluentCollection col = new FluentCollection()
				.addOrdering(new FluentCollectionOrdering().by("a"))
				.addOrdering(new FluentCollectionOrdering().by("b"))
				.clearOrdering();
		assertEquals(0, col.get().getOrdering().size());
	}

	@Test
	void findOrderingReturnsMatch() {
		FluentCollection col = new FluentCollection().addOrdering(new FluentCollectionOrdering().by("name"));
		assertThat(col.findOrdering("name"), is(notNullValue()));
	}

	@Test
	void findOrderingReturnsNullWhenMissing() {
		assertThat(new FluentCollection().findOrdering("missing"), is(nullValue()));
	}

	@Test
	void addUniqueConstraintAddsEntry() {
		FluentCollection col = new FluentCollection().addUniqueConstraint(new FluentCollectionUniqueConstraint().name("uc1"));
		assertEquals(1, col.get().getUniqueConstraints().size());
	}

	@Test
	void removeUniqueConstraintRemovesEntry() {
		FluentCollection col = new FluentCollection()
				.addUniqueConstraint(new FluentCollectionUniqueConstraint().name("uc1"))
				.removeUniqueConstraint("uc1");
		assertEquals(0, col.get().getUniqueConstraints().size());
	}

	@Test
	void clearUniqueConstraintsRemovesAll() {
		FluentCollection col = new FluentCollection()
				.addUniqueConstraint(new FluentCollectionUniqueConstraint().name("uc1"))
				.addUniqueConstraint(new FluentCollectionUniqueConstraint().name("uc2"))
				.clearUniqueConstraints();
		assertEquals(0, col.get().getUniqueConstraints().size());
	}

	@Test
	void findUniqueConstraintReturnsMatch() {
		FluentCollection col = new FluentCollection().addUniqueConstraint(new FluentCollectionUniqueConstraint().name("uc1"));
		assertThat(col.findUniqueConstraint("uc1"), is(notNullValue()));
	}

	@Test
	void findUniqueConstraintReturnsNullWhenMissing() {
		assertThat(new FluentCollection().findUniqueConstraint("missing"), is(nullValue()));
	}

	@Test
	void fromCopiesType() {
		CollectionImpl src = new CollectionImpl();
		src.setType(CollectionType.composition);
		assertThat(new FluentCollection().from(src).get().getType(), is(CollectionType.composition));
	}

	@Test
	void fromCopiesMaxCardinalityWhenSet() {
		CollectionImpl src = new CollectionImpl();
		src.setMaxCardinality(Integer.valueOf(5));
		assertThat(new FluentCollection().from(src).get().getMaxCardinality(), is(Integer.valueOf(5)));
	}

	@Test
	void fromSkipsMaxCardinalityWhenNull() {
		CollectionImpl src = new CollectionImpl();
		src.setMaxCardinality(null);
		assertThat(new FluentCollection().from(src).get().getMaxCardinality(), is(nullValue()));
	}

	@Test
	void fromCopiesOwnerDatabaseIndexWhenSet() {
		CollectionImpl src = new CollectionImpl();
		src.setOwnerDatabaseIndex(Boolean.TRUE);
		assertThat(new FluentCollection().from(src).get().getOwnerDatabaseIndex(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesElementDatabaseIndexWhenSet() {
		CollectionImpl src = new CollectionImpl();
		src.setElementDatabaseIndex(Boolean.TRUE);
		assertThat(new FluentCollection().from(src).get().getElementDatabaseIndex(), is(Boolean.TRUE));
	}

	@Test
	void fromCopiesOrdering() {
		CollectionImpl src = new CollectionImpl();
		src.getOrdering().add(new FluentCollectionOrdering().by("name").get());
		assertEquals(1, new FluentCollection().from(src).get().getOrdering().size());
	}

	@Test
	void fromCopiesUniqueConstraints() {
		CollectionImpl src = new CollectionImpl();
		src.getUniqueConstraints().add(new FluentCollectionUniqueConstraint().name("uc1").get());
		assertEquals(1, new FluentCollection().from(src).get().getUniqueConstraints().size());
	}
}

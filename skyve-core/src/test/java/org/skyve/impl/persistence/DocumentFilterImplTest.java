package org.skyve.impl.persistence;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.persistence.DocumentFilter;

@ExtendWith(MockitoExtension.class)
class DocumentFilterImplTest {

	@Mock
	private AbstractDocumentQuery mockQuery;

	private DocumentFilterImpl filter;

	@BeforeEach
	void setUp() {
		filter = new DocumentFilterImpl(mockQuery, null);
	}

	// ---- isEmpty / toString -------------------------------------------------

	@Test
	void isEmptyReturnsTrueForNewFilter() {
		assertTrue(filter.isEmpty());
	}

	@Test
	void isEmptyReturnsFalseAfterAddNull() {
		filter.addNull("field");
		assertFalse(filter.isEmpty());
	}

	@Test
	void toStringReturnsEmptyStringForNewFilter() {
		assertEquals("", filter.toString());
	}

	// ---- addNull / addNotNull -----------------------------------------------

	@Test
	void addNullAppendsIsNullClause() {
		filter.addNull("name");
		assertEquals("bean.name IS NULL", filter.toString());
	}

	@Test
	void addNotNullAppendsIsNotNullClause() {
		filter.addNotNull("status");
		assertEquals("bean.status IS NOT NULL", filter.toString());
	}

	@Test
	void addNullTwiceInsertsAndBetweenClauses() {
		filter.addNull("a");
		filter.addNull("b");
		assertEquals("bean.a IS NULL AND bean.b IS NULL", filter.toString());
	}

	@Test
	void addNullReturnsThis() {
		DocumentFilter result = filter.addNull("field");
		assertSame(filter, result);
	}

	@Test
	void addNotNullReturnsThis() {
		DocumentFilter result = filter.addNotNull("field");
		assertSame(filter, result);
	}

	// ---- addAliasedNull / addAliasedNotNull ---------------------------------

	@Test
	void addAliasedNullUsesSuppliedAlias() {
		filter.addAliasedNull("t", "name");
		assertEquals("t.name IS NULL", filter.toString());
	}

	@Test
	void addAliasedNotNullUsesSuppliedAlias() {
		filter.addAliasedNotNull("t", "code");
		assertEquals("t.code IS NOT NULL", filter.toString());
	}

	// ---- addExpression ------------------------------------------------------

	@Test
	void addExpressionAppendsExpression() {
		filter.addExpression("bean.amount > 100");
		assertEquals("bean.amount > 100", filter.toString());
	}

	@Test
	void addExpressionTwiceInsertsAnd() {
		filter.addExpression("bean.a = 1");
		filter.addExpression("bean.b = 2");
		assertEquals("bean.a = 1 AND bean.b = 2", filter.toString());
	}

	// ---- addAnd -------------------------------------------------------------

	@Test
	void addAndCombinesFiltersWithAnd() {
		DocumentFilterImpl other = new DocumentFilterImpl(mockQuery, null);
		other.addNull("b");
		filter.addNull("a");
		filter.addAnd(other);
		assertEquals("bean.a IS NULL AND (bean.b IS NULL)", filter.toString());
	}

	@Test
	void addAndOnEmptyFilterAppendsOther() {
		DocumentFilterImpl other = new DocumentFilterImpl(mockQuery, null);
		other.addNull("b");
		filter.addAnd(other);
		assertEquals("(bean.b IS NULL)", filter.toString());
	}

	// ---- addOr --------------------------------------------------------------

	@Test
	void addOrCombinesFiltersWithOr() {
		DocumentFilterImpl other = new DocumentFilterImpl(mockQuery, null);
		other.addNull("b");
		filter.addNull("a");
		filter.addOr(other);
		assertEquals("(bean.a IS NULL) OR (bean.b IS NULL)", filter.toString());
	}

	@Test
	void addOrOnEmptyFilterJustAppendsOther() {
		DocumentFilterImpl other = new DocumentFilterImpl(mockQuery, null);
		other.addNull("b");
		filter.addOr(other);
		assertEquals("(bean.b IS NULL)", filter.toString());
	}

	// ---- addEquals ----------------------------------------------------------

	@Test
	void addEqualsAppendsEqualsClause() {
		filter.addEquals("status", "active");
		assertEquals("bean.status = :param0", filter.toString());
	}

	@Test
	void addEqualsMultipleUsesIncrementingParamNames() {
		filter.addEquals("a", "x");
		filter.addEquals("b", "y");
		assertEquals("bean.a = :param0 AND bean.b = :param1", filter.toString());
	}

	// ---- addIn --------------------------------------------------------------

	@Test
	void addInAppendsInClauseWithSingleOperand() {
		filter.addIn("status", "active");
		assertEquals("bean.status in (:param0)", filter.toString());
	}

	@Test
	void addInAppendsInClauseWithMultipleOperands() {
		filter.addIn("status", "active", "pending");
		assertEquals("bean.status in (:param0,:param1)", filter.toString());
	}

	@Test
	void addNotInAppendsNotInClause() {
		filter.addNotIn("status", "deleted");
		assertEquals("bean.status not in (:param0)", filter.toString());
	}

	// ---- addNotEquals -------------------------------------------------------

	@Test
	void addNotEqualsAppendsNotEqualsClause() {
		filter.addNotEquals("status", "archived");
		assertEquals("bean.status != :param0", filter.toString());
	}

	// ---- addGreaterThan / addGreaterThanOrEqualTo ---------------------------

	@Test
	void addGreaterThanAppendsGreaterThanClause() {
		filter.addGreaterThan("amount", Integer.valueOf(100));
		assertEquals("bean.amount > :param0", filter.toString());
	}

	@Test
	void addGreaterThanOrEqualToAppendsGeClause() {
		filter.addGreaterThanOrEqualTo("amount", Integer.valueOf(50));
		assertEquals("bean.amount >= :param0", filter.toString());
	}

	// ---- addLessThan / addLessThanOrEqualTo --------------------------------

	@Test
	void addLessThanAppendsLessThanClause() {
		filter.addLessThan("amount", Integer.valueOf(200));
		assertEquals("bean.amount < :param0", filter.toString());
	}

	@Test
	void addLessThanOrEqualToAppendsLeClause() {
		filter.addLessThanOrEqualTo("amount", Integer.valueOf(200));
		assertEquals("bean.amount <= :param0", filter.toString());
	}

	// ---- addLike / addNotLike ----------------------------------------------

	@Test
	void addLikeAppendsLikeClause() {
		filter.addLike("name", "%smith%");
		assertEquals("bean.name like :param0", filter.toString());
	}

	@Test
	void addNotLikeAppendsNotLikeClause() {
		filter.addNotLike("name", "%test%");
		assertEquals("bean.name not like :param0", filter.toString());
	}

	// ---- addAliasedEquals --------------------------------------------------

	@Test
	void addAliasedEqualsUsesSuppliedAlias() {
		filter.addAliasedEquals("t", "code", "ABC");
		assertEquals("t.code = :param0", filter.toString());
	}

	// ---- addNullOrEquals / addNullOrNotEquals ------------------------------

	@Test
	void addNullOrEqualsAppendsNullOrEqualsClause() {
		filter.addNullOrEquals("status", "active");
		assertEquals("(bean.status IS NULL OR bean.status = :param0)", filter.toString());
	}

	@Test
	void addNullOrNotEqualsAppendsNullOrNotEqualsClause() {
		filter.addNullOrNotEquals("status", "archived");
		assertEquals("(bean.status IS NULL OR bean.status != :param0)", filter.toString());
	}

	@Test
	void addNullOrGreaterThanAppendsNullOrGtClause() {
		filter.addNullOrGreaterThan("amount", Integer.valueOf(10));
		assertEquals("(bean.amount IS NULL OR bean.amount > :param0)", filter.toString());
	}

	@Test
	void addNullOrLessThanAppendsNullOrLtClause() {
		filter.addNullOrLessThan("amount", Integer.valueOf(100));
		assertEquals("(bean.amount IS NULL OR bean.amount < :param0)", filter.toString());
	}

	@Test
	void addNullOrLikeAppendsNullOrLikeClause() {
		filter.addNullOrLike("name", "%jones%");
		assertEquals("(bean.name IS NULL OR bean.name like :param0)", filter.toString());
	}

	@Test
	void addNullOrNotLikeAppendsNullOrNotLikeClause() {
		filter.addNullOrNotLike("name", "%jones%");
		assertEquals("(bean.name IS NULL OR bean.name not like :param0)", filter.toString());
	}

	@Test
	void addNullOrGreaterThanOrEqualToAppendsNullOrGeClause() {
		filter.addNullOrGreaterThanOrEqualTo("amount", Integer.valueOf(10));
		assertEquals("(bean.amount IS NULL OR bean.amount >= :param0)", filter.toString());
	}

	@Test
	void addNullOrLessThanOrEqualToAppendsNullOrLeClause() {
		filter.addNullOrLessThanOrEqualTo("amount", Integer.valueOf(100));
		assertEquals("(bean.amount IS NULL OR bean.amount <= :param0)", filter.toString());
	}

	// ---- addBetween --------------------------------------------------------

	@Test
	void addBetweenAppendsBetweenClause() {
		filter.addBetween("amount", Integer.valueOf(10), Integer.valueOf(100));
		assertEquals("bean.amount BETWEEN :param0 and :param1", filter.toString());
	}

	@Test
	void addBetweenTwiceInsertsAnd() {
		filter.addBetween("a", Integer.valueOf(1), Integer.valueOf(5));
		filter.addBetween("b", Integer.valueOf(10), Integer.valueOf(50));
		assertEquals("bean.a BETWEEN :param0 and :param1 AND bean.b BETWEEN :param2 and :param3", filter.toString());
	}

	// ---- addCollectionSizeEquals / NotEquals / GreaterThan / LessThan ------

	@Test
	void addCollectionSizeEqualsAppendsCorrectClause() {
		filter.addCollectionSizeEquals("contacts", 3);
		assertEquals("bean.contacts.size = :param0", filter.toString());
	}

	@Test
	void addCollectionSizeNotEqualsAppendsCorrectClause() {
		filter.addCollectionSizeNotEquals("contacts", 0);
		assertEquals("bean.contacts.size != :param0", filter.toString());
	}

	@Test
	void addCollectionSizeGreaterThanAppendsCorrectClause() {
		filter.addCollectionSizeGreaterThan("items", 0);
		assertEquals("bean.items.size > :param0", filter.toString());
	}

	@Test
	void addCollectionSizeLessThanAppendsCorrectClause() {
		filter.addCollectionSizeLessThan("items", 5);
		assertEquals("bean.items.size < :param0", filter.toString());
	}

	// ---- addMemberOfCollection / addNotMemberOfCollection ------------------

	@Test
	void addMemberOfCollectionAppendsCorrectClause() {
		DocumentFilterImpl memberFilter = new DocumentFilterImpl(mockQuery, null);
		org.skyve.domain.Bean beanMock = org.mockito.Mockito.mock(org.skyve.domain.Bean.class);
		memberFilter.addMemberOfCollection("contacts", beanMock);
		assertEquals(":param0 member of bean.contacts", memberFilter.toString());
	}

	@Test
	void addNotMemberOfCollectionAppendsCorrectClause() {
		DocumentFilterImpl notMemberFilter = new DocumentFilterImpl(mockQuery, null);
		org.skyve.domain.Bean beanMock = org.mockito.Mockito.mock(org.skyve.domain.Bean.class);
		notMemberFilter.addNotMemberOfCollection("contacts", beanMock);
		assertEquals(":param0 not member of bean.contacts", notMemberFilter.toString());
	}

	// ---- addEquals(Geometry) -----------------------------------------------

	@Test
	void addEqualsGeometryAppendsGeometryEqualsClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addEquals("location", geom);
		assertEquals("equals(bean.location, :param0) = true", filter.toString());
	}

	@Test
	void addIntersectsGeometryAppendsCorrectClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (0 0)");
		filter.addIntersects("location", geom);
		assertEquals("intersects(bean.location, :param0) = true", filter.toString());
	}

	@Test
	void addDisjointGeometryAppendsCorrectClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addDisjoint("location", geom);
		assertEquals("disjoint(bean.location, :param0) = true", filter.toString());
	}

	@Test
	void addTouchesGeometryAppendsCorrectClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addTouches("location", geom);
		assertEquals("touches(bean.location, :param0) = true", filter.toString());
	}

	@Test
	void addCrossesGeometryAppendsCorrectClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addCrosses("location", geom);
		assertEquals("crosses(bean.location, :param0) = true", filter.toString());
	}

	@Test
	void addWithinGeometryAppendsCorrectClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addWithin("location", geom);
		assertEquals("within(bean.location, :param0) = true", filter.toString());
	}

	@Test
	void addContainsGeometryAppendsCorrectClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addContains("location", geom);
		assertEquals("contains(bean.location, :param0) = true", filter.toString());
	}

	@Test
	void addOverlapsGeometryAppendsCorrectClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addOverlaps("location", geom);
		assertEquals("overlaps(bean.location, :param0) = true", filter.toString());
	}

	@Test
	void addNullOrEqualsGeometryAppendsNullOrClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addNullOrEquals("location", geom);
		assertEquals("(bean.location IS NULL OR equals(bean.location, :param0) = true)", filter.toString());
	}

	@Test
	void addNullOrDisjointGeometryAppendsNullOrClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addNullOrDisjoint("location", geom);
		assertEquals("(bean.location IS NULL OR disjoint(bean.location, :param0) = true)", filter.toString());
	}

	@Test
	void addNullOrIntersectsGeometryAppendsNullOrClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addNullOrIntersects("location", geom);
		assertEquals("(bean.location IS NULL OR intersects(bean.location, :param0) = true)", filter.toString());
	}

	@Test
	void addNullOrTouchesGeometryAppendsNullOrClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addNullOrTouches("location", geom);
		assertEquals("(bean.location IS NULL OR touches(bean.location, :param0) = true)", filter.toString());
	}

	@Test
	void addNullOrCrossesGeometryAppendsNullOrClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addNullOrCrosses("location", geom);
		assertEquals("(bean.location IS NULL OR crosses(bean.location, :param0) = true)", filter.toString());
	}

	@Test
	void addNullOrWithinGeometryAppendsNullOrClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addNullOrWithin("location", geom);
		assertEquals("(bean.location IS NULL OR within(bean.location, :param0) = true)", filter.toString());
	}

	@Test
	void addNullOrContainsGeometryAppendsNullOrClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addNullOrContains("location", geom);
		assertEquals("(bean.location IS NULL OR contains(bean.location, :param0) = true)", filter.toString());
	}

	@Test
	void addNullOrOverlapsGeometryAppendsNullOrClause() throws Exception {
		org.locationtech.jts.geom.Geometry geom = new org.locationtech.jts.io.WKTReader().read("POINT (1 2)");
		filter.addNullOrOverlaps("location", geom);
		assertEquals("(bean.location IS NULL OR overlaps(bean.location, :param0) = true)", filter.toString());
	}

	// ---- PostgreSQL lower-case path ----------------------------------------

	@Test
	void addEqualsStringWithPostgresUsesLower() {
		DocumentFilterImpl pgFilter = new DocumentFilterImpl(mockQuery, org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS.postgresql);
		pgFilter.addEquals("name", "smith");
		assertEquals("lower(bean.name) = lower(:param0)", pgFilter.toString());
	}

	@Test
	void addEqualsStringBizIdWithPostgresSkipsLower() {
		DocumentFilterImpl pgFilter = new DocumentFilterImpl(mockQuery, org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS.postgresql);
		pgFilter.addEquals("bizId", "some-id");
		assertEquals("bean.bizId = :param0", pgFilter.toString());
	}

	@Test
	void addInStringWithPostgresUsesLower() {
		DocumentFilterImpl pgFilter = new DocumentFilterImpl(mockQuery, org.skyve.impl.persistence.hibernate.dialect.SkyveDialect.RDBMS.postgresql);
		pgFilter.addIn("name", "alice", "bob");
		assertEquals("lower(bean.name) in (lower(:param0),lower(:param1))", pgFilter.toString());
	}

}

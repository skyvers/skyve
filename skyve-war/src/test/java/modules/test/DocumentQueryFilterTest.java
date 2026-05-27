package modules.test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Date;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.skyve.domain.types.Decimal2;
import org.skyve.metadata.view.model.list.DocumentQueryListModel;
import org.skyve.metadata.view.model.list.Filter;
import modules.test.domain.AllAttributesPersistent.Enum3;

/**
 * Tests for DocumentQueryFilter through DocumentQueryListModel.
 * Covers all the delegate methods in DocumentQueryFilter.
 */
class DocumentQueryFilterTest extends AbstractSkyveTest {

	private static final Geometry POINT = new GeometryFactory().createPoint(new Coordinate(0, 0));
	private Filter filter;
	private Filter filter2; // second filter for addAnd/addOr

	@BeforeEach
	void setUpFilter() {
		// Use qH query (Hierarchical document) as a simple test query
		DocumentQueryListModel<?> model = new DocumentQueryListModel<>(m.getNullSafeMetaDataQuery("qH"));
		model.postConstruct(c, false);
		filter = model.getFilter();
		filter2 = model.newFilter();
	}

	// ===== isEmpty =====

	@Test
	void isEmptyReturnsTrueInitially() {
		assertTrue(filter.isEmpty());
	}

	// ===== addNull / addNotNull =====

	@Test
	void addNullSetsNotEmpty() {
		filter.addNull("bizId");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotNullSetsNotEmpty() {
		filter.addNotNull("bizId");
		assertFalse(filter.isEmpty());
	}

	// ===== addAnd / addOr =====

	@Test
	void addAndSetsNotEmpty() {
		filter2.addNull("bizId");
		filter.addAnd(filter2);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addOrSetsNotEmpty() {
		filter2.addNull("bizId");
		filter.addOr(filter2);
		assertFalse(filter.isEmpty());
	}

	// ===== addEquals =====

	@Test
	void addEqualsStringSetsNotEmpty() {
		filter.addEquals("bizKey", "test");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsDateSetsNotEmpty() {
		filter.addEquals("bizKey", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsIntegerSetsNotEmpty() {
		filter.addEquals("bizKey", Integer.valueOf(1));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsLongSetsNotEmpty() {
		filter.addEquals("bizKey", Long.valueOf(1L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsDecimalSetsNotEmpty() {
		filter.addEquals("bizKey", new Decimal2("1.23"));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsBooleanSetsNotEmpty() {
		filter.addEquals("bizKey", Boolean.TRUE);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addEqualsGeometrySetsNotEmpty() {
		filter.addEquals("bizKey", POINT);
		assertFalse(filter.isEmpty());
	}

	// ===== addNotEquals =====

	@Test
	void addNotEqualsStringSetsNotEmpty() {
		filter.addNotEquals("bizKey", "x");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsDateSetsNotEmpty() {
		filter.addNotEquals("bizKey", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsIntegerSetsNotEmpty() {
		filter.addNotEquals("bizKey", Integer.valueOf(2));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsLongSetsNotEmpty() {
		filter.addNotEquals("bizKey", Long.valueOf(2L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsDecimalSetsNotEmpty() {
		filter.addNotEquals("bizKey", new Decimal2("5.00"));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsBooleanSetsNotEmpty() {
		filter.addNotEquals("bizKey", Boolean.FALSE);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsGeometrySetsNotEmpty() {
		filter.addNotEquals("bizKey", POINT);
		assertFalse(filter.isEmpty());
	}

	// ===== addEqualsIgnoreCase / addNotEqualsIgnoreCase =====

	@Test
	void addEqualsIgnoreCaseSetsNotEmpty() {
		filter.addEqualsIgnoreCase("bizKey", "TEST");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsIgnoreCaseSetsNotEmpty() {
		filter.addNotEqualsIgnoreCase("bizKey", "x");
		assertFalse(filter.isEmpty());
	}

	// ===== addContains / addNotContains =====

	@Test
	void addContainsStringSetsNotEmpty() {
		filter.addContains("bizKey", "est");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotContainsStringSetsNotEmpty() {
		filter.addNotContains("bizKey", "bad");
		assertFalse(filter.isEmpty());
	}

	// ===== addStartsWith / addNotStartsWith =====

	@Test
	void addStartsWithSetsNotEmpty() {
		filter.addStartsWith("bizKey", "te");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotStartsWithSetsNotEmpty() {
		filter.addNotStartsWith("bizKey", "no");
		assertFalse(filter.isEmpty());
	}

	// ===== addEndsWith / addNotEndsWith =====

	@Test
	void addEndsWithSetsNotEmpty() {
		filter.addEndsWith("bizKey", "st");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEndsWithSetsNotEmpty() {
		filter.addNotEndsWith("bizKey", "no");
		assertFalse(filter.isEmpty());
	}

	// ===== addGreaterThan =====

	@Test
	void addGreaterThanStringSetsNotEmpty() {
		filter.addGreaterThan("bizKey", "a");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanDateSetsNotEmpty() {
		filter.addGreaterThan("bizKey", new Date(0L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanIntegerSetsNotEmpty() {
		filter.addGreaterThan("bizKey", Integer.valueOf(5));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanLongSetsNotEmpty() {
		filter.addGreaterThan("bizKey", Long.valueOf(5L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanDecimalSetsNotEmpty() {
		filter.addGreaterThan("bizKey", new Decimal2("5.00"));
		assertFalse(filter.isEmpty());
	}

	// ===== addGreaterThanOrEqualTo =====

	@Test
	void addGreaterThanOrEqualToStringSetsNotEmpty() {
		filter.addGreaterThanOrEqualTo("bizKey", "a");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToDateSetsNotEmpty() {
		filter.addGreaterThanOrEqualTo("bizKey", new Date(0L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToIntegerSetsNotEmpty() {
		filter.addGreaterThanOrEqualTo("bizKey", Integer.valueOf(5));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToLongSetsNotEmpty() {
		filter.addGreaterThanOrEqualTo("bizKey", Long.valueOf(5L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addGreaterThanOrEqualToDecimalSetsNotEmpty() {
		filter.addGreaterThanOrEqualTo("bizKey", new Decimal2("5.00"));
		assertFalse(filter.isEmpty());
	}

	// ===== addLessThan =====

	@Test
	void addLessThanStringSetsNotEmpty() {
		filter.addLessThan("bizKey", "z");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanDateSetsNotEmpty() {
		filter.addLessThan("bizKey", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanIntegerSetsNotEmpty() {
		filter.addLessThan("bizKey", Integer.valueOf(100));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanLongSetsNotEmpty() {
		filter.addLessThan("bizKey", Long.valueOf(100L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanDecimalSetsNotEmpty() {
		filter.addLessThan("bizKey", new Decimal2("9.99"));
		assertFalse(filter.isEmpty());
	}

	// ===== addLessThanOrEqualTo =====

	@Test
	void addLessThanOrEqualToStringSetsNotEmpty() {
		filter.addLessThanOrEqualTo("bizKey", "z");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToDateSetsNotEmpty() {
		filter.addLessThanOrEqualTo("bizKey", new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToIntegerSetsNotEmpty() {
		filter.addLessThanOrEqualTo("bizKey", Integer.valueOf(100));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToLongSetsNotEmpty() {
		filter.addLessThanOrEqualTo("bizKey", Long.valueOf(100L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addLessThanOrEqualToDecimalSetsNotEmpty() {
		filter.addLessThanOrEqualTo("bizKey", new Decimal2("9.99"));
		assertFalse(filter.isEmpty());
	}

	// ===== addBetween =====

	@Test
	void addBetweenStringSetsNotEmpty() {
		filter.addBetween("bizKey", "a", "z");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenDateSetsNotEmpty() {
		filter.addBetween("bizKey", new Date(0L), new Date());
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenIntegerSetsNotEmpty() {
		filter.addBetween("bizKey", Integer.valueOf(1), Integer.valueOf(10));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenLongSetsNotEmpty() {
		filter.addBetween("bizKey", Long.valueOf(1L), Long.valueOf(100L));
		assertFalse(filter.isEmpty());
	}

	@Test
	void addBetweenDecimalSetsNotEmpty() {
		filter.addBetween("bizKey", new Decimal2("1.00"), new Decimal2("9.99"));
		assertFalse(filter.isEmpty());
	}

	// ===== addIn / addNotIn =====

	@Test
	void addInWithValuesSetsNotEmpty() {
		filter.addIn("bizKey", "a", "b");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addInWithEmptyValuesSetsNotEmpty() {
		filter.addIn("bizKey");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotInWithValuesSetsNotEmpty() {
		filter.addNotIn("bizKey", "x");
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotInWithEmptyValuesSetsNotEmpty() {
		filter.addNotIn("bizKey");
		assertFalse(filter.isEmpty());
	}

	// ===== geometry predicates =====

	@Test
	void addWithinSetsNotEmpty() {
		filter.addWithin("bizKey", POINT);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addContainsGeometrySetsNotEmpty() {
		filter.addContains("bizKey", POINT);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addCrossesSetsNotEmpty() {
		filter.addCrosses("bizKey", POINT);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addDisjointSetsNotEmpty() {
		filter.addDisjoint("bizKey", POINT);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addIntersectsSetsNotEmpty() {
		filter.addIntersects("bizKey", POINT);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addOverlapsSetsNotEmpty() {
		filter.addOverlaps("bizKey", POINT);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addTouchesSetsNotEmpty() {
		filter.addTouches("bizKey", POINT);
		assertFalse(filter.isEmpty());
	}

	// ===== addEquals(Enum) / addNotEquals(Enum) =====

	@Test
	void addEqualsEnumSetsNotEmpty() {
		filter.addEquals("enum3", Enum3.one);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addNotEqualsEnumSetsNotEmpty() {
		filter.addNotEquals("enum3", Enum3.two);
		assertFalse(filter.isEmpty());
	}

	// ===== addTagged =====

	@Test
	void addTaggedWithTaggedTrueSetsNotEmpty() {
		filter.addTagged("someTagId", true);
		assertFalse(filter.isEmpty());
	}

	@Test
	void addTaggedWithTaggedFalseSetsNotEmpty() {
		filter.addTagged("someTagId", false);
		assertFalse(filter.isEmpty());
	}
}

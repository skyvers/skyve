package org.skyve.metadata.view.model.list;

import java.util.Date;

import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.types.Decimal;

public interface Filter {
	public void addAnd(Filter filter);
	public void addOr(Filter filter);
	
	public void addTagged(String tagId, boolean tagged);	

	public void addNull(String binding);
	public void addNotNull(String binding);
	
	public void addEquals(String binding, String value);
	public void addEquals(String binding, Date value);
	public void addEquals(String binding, Integer value);
	public void addEquals(String binding, Long value);
	public void addEquals(String binding, Decimal value);
	public void addEquals(String binding, Boolean value);
	public void addEquals(String binding, Enum<?> value);
	public void addEquals(String binding, Geometry value);
	public void addNotEquals(String binding, String value);
	public void addNotEquals(String binding, Date value);
	public void addNotEquals(String binding, Integer value);
	public void addNotEquals(String binding, Long value);
	public void addNotEquals(String binding, Decimal value);
	public void addNotEquals(String binding, Boolean value);
	public void addNotEquals(String binding, Enum<?> value);
	public void addNotEquals(String binding, Geometry value);
	public void addEqualsIgnoreCase(String binding, String value);
	public void addNotEqualsIgnoreCase(String binding, String value);
	public void addContains(String binding, String value);
	public void addNotContains(String binding, String value);
	public void addStartsWith(String binding, String value);
	public void addNotStartsWith(String binding, String value);
	public void addEndsWith(String binding, String value);
	public void addNotEndsWith(String binding, String value);

	public void addGreaterThan(String binding, String value);
	public void addGreaterThan(String binding, Date value);
	public void addGreaterThan(String binding, Integer value);
	public void addGreaterThan(String binding, Long value);
	public void addGreaterThan(String binding, Decimal value);

	public void addGreaterThanOrEqualTo(String binding, String value);
	public void addGreaterThanOrEqualTo(String binding, Date value);
	public void addGreaterThanOrEqualTo(String binding, Integer value);
	public void addGreaterThanOrEqualTo(String binding, Long value);
	public void addGreaterThanOrEqualTo(String binding, Decimal value);

	public void addLessThan(String binding, String value);
	public void addLessThan(String binding, Date value);
	public void addLessThan(String binding, Integer value);
	public void addLessThan(String binding, Long value);
	public void addLessThan(String binding, Decimal value);
	
	public void addLessThanOrEqualTo(String binding, String value);
	public void addLessThanOrEqualTo(String binding, Date value);
	public void addLessThanOrEqualTo(String binding, Integer value);
	public void addLessThanOrEqualTo(String binding, Long value);
	public void addLessThanOrEqualTo(String binding, Decimal value);

	public void addBetween(String binding, String start, String end);
	public void addBetween(String binding, Date start, Date end);
	public void addBetween(String binding, Integer start, Integer end);
	public void addBetween(String binding, Long start, Long end);
	public void addBetween(String binding, Decimal start, Decimal end);

	public void addWithin(String binding, Geometry value);
	public void addContains(String binding, Geometry value);
	public void addCrosses(String binding, Geometry value);
	public void addDisjoint(String binding, Geometry value);
	public void addIntersects(String binding, Geometry value);
	public void addOverlaps(String binding, Geometry value);
	public void addTouches(String binding, Geometry value);
	// TODO need not versions of these geospatial relations
	
	public boolean isEmpty();
}

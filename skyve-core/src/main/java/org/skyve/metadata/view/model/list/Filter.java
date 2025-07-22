package org.skyve.metadata.view.model.list;

import java.util.Date;

import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.types.Decimal;

import jakarta.annotation.Nonnull;

public interface Filter {
	public void addAnd(@Nonnull Filter filter);
	public void addOr(@Nonnull Filter filter);
	
	public void addTagged(@Nonnull String tagId, boolean tagged);	

	public void addNull(@Nonnull String binding);
	public void addNotNull(@Nonnull String binding);
	
	public void addEquals(@Nonnull String binding, @Nonnull String value);
	public void addEquals(@Nonnull String binding, @Nonnull Date value);
	public void addEquals(@Nonnull String binding, @Nonnull Integer value);
	public void addEquals(@Nonnull String binding, @Nonnull Long value);
	public void addEquals(@Nonnull String binding, @Nonnull Decimal value);
	public void addEquals(@Nonnull String binding, Boolean value);
	public void addEquals(@Nonnull String binding, @Nonnull Enum<?> value);
	public void addEquals(@Nonnull String binding, @Nonnull Geometry value);
	public void addNotEquals(@Nonnull String binding, @Nonnull String value);
	public void addNotEquals(@Nonnull String binding, @Nonnull Date value);
	public void addNotEquals(@Nonnull String binding, @Nonnull Integer value);
	public void addNotEquals(@Nonnull String binding, @Nonnull Long value);
	public void addNotEquals(@Nonnull String binding, @Nonnull Decimal value);
	public void addNotEquals(@Nonnull String binding, @Nonnull Boolean value);
	public void addNotEquals(@Nonnull String binding, @Nonnull Enum<?> value);
	public void addNotEquals(@Nonnull String binding, @Nonnull Geometry value);
	public void addEqualsIgnoreCase(@Nonnull String binding, @Nonnull String value);
	public void addNotEqualsIgnoreCase(@Nonnull String binding, @Nonnull String value);
	public void addContains(@Nonnull String binding, @Nonnull String value);
	public void addNotContains(@Nonnull String binding, @Nonnull String value);
	public void addStartsWith(@Nonnull String binding, @Nonnull String value);
	public void addNotStartsWith(@Nonnull String binding, @Nonnull String value);
	public void addEndsWith(@Nonnull String binding, @Nonnull String value);
	public void addNotEndsWith(@Nonnull String binding, @Nonnull String value);

	public void addGreaterThan(@Nonnull String binding, @Nonnull String value);
	public void addGreaterThan(@Nonnull String binding, @Nonnull Date value);
	public void addGreaterThan(@Nonnull String binding, @Nonnull Integer value);
	public void addGreaterThan(@Nonnull String binding, @Nonnull Long value);
	public void addGreaterThan(@Nonnull String binding, @Nonnull Decimal value);

	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull String value);
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Date value);
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Integer value);
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Long value);
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Decimal value);

	public void addLessThan(@Nonnull String binding, @Nonnull String value);
	public void addLessThan(@Nonnull String binding, @Nonnull Date value);
	public void addLessThan(@Nonnull String binding, @Nonnull Integer value);
	public void addLessThan(@Nonnull String binding, @Nonnull Long value);
	public void addLessThan(@Nonnull String binding, @Nonnull Decimal value);
	
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull String value);
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Date value);
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Integer value);
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Long value);
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Decimal value);

	public void addBetween(@Nonnull String binding, @Nonnull String start, @Nonnull String end);
	public void addBetween(@Nonnull String binding, @Nonnull Date start, @Nonnull Date end);
	public void addBetween(@Nonnull String binding, @Nonnull Integer start, @Nonnull Integer end);
	public void addBetween(@Nonnull String binding, @Nonnull Long start, @Nonnull Long end);
	public void addBetween(@Nonnull String binding, @Nonnull Decimal start, @Nonnull Decimal end);

	public void addIn(@Nonnull String binding, Object... values);
	public void addNotIn(@Nonnull String binding, Object... values);
	
	public void addWithin(@Nonnull String binding, @Nonnull Geometry value);
	public void addContains(@Nonnull String binding, @Nonnull Geometry value);
	public void addCrosses(@Nonnull String binding, @Nonnull Geometry value);
	public void addDisjoint(@Nonnull String binding, @Nonnull Geometry value);
	public void addIntersects(@Nonnull String binding, Geometry value);
	public void addOverlaps(@Nonnull String binding, @Nonnull Geometry value);
	public void addTouches(@Nonnull String binding, @Nonnull Geometry value);
	// TODO need not versions of these geospatial relations
	
	public boolean isEmpty();
}

package org.skyve.metadata.view.model.list;

import java.util.Date;

import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.types.Decimal;

import jakarta.annotation.Nonnull;

/**
 * Composable filter predicate applied to a {@link ListModel} or query-backed list.
 *
 * <p>A {@code Filter} is built by adding typed criteria via the {@code add*} methods and
 * composing sub-filters with {@link #addAnd}/{@link #addOr}. The framework calls
 * {@link ListModel#getFilter()} to obtain the current filter and applies it during
 * {@link ListModel#fetch()}.
 *
 * <p>Each {@code add*} method appends a criterion on the given binding path. All criteria
 * added directly to one {@code Filter} instance are combined with logical AND; use
 * {@link #addOr(Filter)} to introduce disjunctions.
 *
 * <p>Obtain a new filter via {@link ListModel#newFilter()} so that the implementation
 * can return the correct concrete type for the underlying data source.
 */
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

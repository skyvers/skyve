package org.skyve.metadata.view.model.list;

import java.util.Date;

import org.skyve.CORE;
import org.skyve.domain.types.Decimal;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;

import com.vividsolutions.jts.geom.Geometry;

public class DocumentQueryFilter implements Filter {
	private DocumentFilter detailFilter;
	private DocumentFilter summaryFilter;
	private User user;
	private boolean empty = true;
	
	DocumentQueryFilter(DocumentFilter detailFilter, DocumentFilter summaryFilter) {
		this.detailFilter = detailFilter;
		this.summaryFilter = summaryFilter;
		user = CORE.getUser();
	}
	
	@Override
	public void addAnd(Filter filter) {
		empty = false;
		detailFilter.addAnd(((DocumentQueryFilter) filter).detailFilter);
		if (summaryFilter != null) {
			summaryFilter.addAnd(((DocumentQueryFilter) filter).summaryFilter);
		}
	}

	@Override
	public void addOr(Filter filter) {
		empty = false;
		detailFilter.addOr(((DocumentQueryFilter) filter).detailFilter);
		if (summaryFilter != null) {
			summaryFilter.addOr(((DocumentQueryFilter) filter).summaryFilter);
		}
	}

	@Override
	public void addTagged(String tagId, boolean tagged) {
		empty = false;
System.out.println(tagId + " add tagged " + tagged);
		StringBuilder sb = new StringBuilder(64);
		sb.append("exists (select 1 from adminTagged as tagged where tagged.tag.bizId = '");
		sb.append(tagId);
		sb.append("' and tagged.bizUserId = '");
		sb.append(user.getId());
		sb.append("' and tagged.taggedBizId = bean.bizId)");

		if (tagged) {
			detailFilter.addExpression(sb.toString());
			if (summaryFilter != null) {
				summaryFilter.addExpression(sb.toString());
			}
		}
		else {
			sb.insert(0, "not ");
			detailFilter.addExpression(sb.toString());
			if (summaryFilter != null) {
				summaryFilter.addExpression(sb.toString());
			}
		}
	}

	@Override
	public void addNull(String binding) {
		empty = false;
System.out.println(binding + " is null");
		detailFilter.addNull(binding);
		if (summaryFilter != null) {
			summaryFilter.addNull(binding);
		}
	}

	@Override
	public void addNotNull(String binding) {
		empty = false;
System.out.println(binding + " is not null");
		detailFilter.addNotNull(binding);
		if (summaryFilter != null) {
			summaryFilter.addNotNull(binding);
		}
	}

	@Override
	public void addEquals(String binding, String value) {
		empty = false;
System.out.println(binding + " equals " + value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addEquals(String binding, Date value) {
		empty = false;
System.out.println(binding + " equals " + value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addEquals(String binding, Integer value) {
		empty = false;
System.out.println(binding + " equals " + value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addEquals(String binding, Long value) {
		empty = false;
System.out.println(binding + " equals " + value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addEquals(String binding, Decimal value) {
		empty = false;
System.out.println(binding + " equals " + value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addEquals(String binding, Boolean value) {
		empty = false;
System.out.println(binding + " equals " + value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addEquals(String binding, Enum<?> value) {
		empty = false;
System.out.println(binding + " equals " + value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}	
	}

	@Override
	public void addEquals(String binding, Geometry value) {
		empty = false;
System.out.println(binding + " equals " + value);
		detailFilter.addEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, String value) {
		empty = false;
System.out.println(binding + " notEquals " + value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Date value) {
		empty = false;
System.out.println(binding + " notEquals " + value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Integer value) {
		empty = false;
System.out.println(binding + " notEquals " + value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Long value) {
		empty = false;
System.out.println(binding + " notEquals " + value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Decimal value) {
		empty = false;
System.out.println(binding + " notEquals " + value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Boolean value) {
		empty = false;
System.out.println(binding + " notEquals " + value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Enum<?> value) {
		empty = false;
System.out.println(binding + " notEquals " + value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addNotEquals(String binding, Geometry value) {
		empty = false;
System.out.println(binding + " notEquals " + value);
		detailFilter.addNotEquals(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotEquals(binding, value);
		}
	}

	@Override
	public void addEqualsIgnoreCase(String binding, String value) {
		empty = false;
System.out.println(binding + " equalIgnoreCase " + value);
		detailFilter.addLike(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, value);
		}
	}

	@Override
	public void addNotEqualsIgnoreCase(String binding, String value) {
		empty = false;
System.out.println(binding + " notEqualIgnoreCase " + value);
		detailFilter.addNotLike(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addNotLike(binding, value);
		}
	}

	@Override
	public void addContains(String binding, String value) {
		empty = false;
System.out.println(binding + " contains " + value);
		String operand = new StringBuilder(32).append('%').append(value).append('%').toString();
		detailFilter.addLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, operand);
		}
	}

	@Override
	public void addNotContains(String binding, String value) {
		empty = false;
System.out.println(binding + " not contains " + value);
		String operand = new StringBuilder(32).append('%').append(value).append('%').toString();
		detailFilter.addNullOrNotLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addNullOrNotLike(binding, operand);
		}
	}

	@Override
	public void addStartsWith(String binding, String value) {
		empty = false;
System.out.println(binding + " starts with " + value);
		String operand = new StringBuilder(32).append(value).append('%').toString();
		detailFilter.addLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, operand);
		}
	}

	@Override
	public void addNotStartsWith(String binding, String value) {
		empty = false;
System.out.println(binding + " not starts with " + value);
		String operand = new StringBuilder(32).append(value).append('%').toString();
		detailFilter.addNullOrNotLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addNullOrNotLike(binding, operand);
		}
	}

	@Override
	public void addEndsWith(String binding, String value) {
		empty = false;
System.out.println(binding + " ends with " + value);
		String operand = new StringBuilder(32).append('%').append(value).toString();
		detailFilter.addLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addLike(binding, operand);
		}
	}

	@Override
	public void addNotEndsWith(String binding, String value) {
		empty = false;
System.out.println(binding + " not ends with " + value);
		String operand = new StringBuilder(32).append('%').append(value).toString();
		detailFilter.addNullOrNotLike(binding, operand);
		if (summaryFilter != null) {
			summaryFilter.addNullOrNotLike(binding, operand);
		}
	}

	@Override
	public void addGreaterThan(String binding, String value) {
		empty = false;
System.out.println(binding + " greater than " + value);
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
	}

	@Override
	public void addGreaterThan(String binding, Date value) {
		empty = false;
System.out.println(binding + " greater than " + value);
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
	}

	@Override
	public void addGreaterThan(String binding, Integer value) {
		empty = false;
System.out.println(binding + " greater than " + value);
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
	}

	@Override
	public void addGreaterThan(String binding, Long value) {
		empty = false;
System.out.println(binding + " greater than " + value);
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
	}

	@Override
	public void addGreaterThan(String binding, Decimal value) {
		empty = false;
System.out.println(binding + " greater than " + value);
		detailFilter.addGreaterThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThan(binding, value);
		}
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, String value) {
		empty = false;
System.out.println(binding + " greater than or equal to " + value);
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Date value) {
		empty = false;
System.out.println(binding + " greater than or equal to " + value);
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Integer value) {
		empty = false;
System.out.println(binding + " greater than or equal to " + value);
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Long value) {
		empty = false;
System.out.println(binding + " greater than or equal to " + value);
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Decimal value) {
		empty = false;
System.out.println(binding + " greater than or equal to " + value);
		detailFilter.addGreaterThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addGreaterThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addLessThan(String binding, String value) {
		empty = false;
System.out.println(binding + " less than " + value);
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
	}

	@Override
	public void addLessThan(String binding, Date value) {
		empty = false;
System.out.println(binding + " less than " + value);
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
	}

	@Override
	public void addLessThan(String binding, Integer value) {
		empty = false;
System.out.println(binding + " less than " + value);
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
	}

	@Override
	public void addLessThan(String binding, Long value) {
		empty = false;
System.out.println(binding + " less than " + value);
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
	}

	@Override
	public void addLessThan(String binding, Decimal value) {
		empty = false;
System.out.println(binding + " less than " + value);
		detailFilter.addLessThan(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThan(binding, value);
		}
	}

	@Override
	public void addLessThanOrEqualTo(String binding, String value) {
		empty = false;
System.out.println(binding + " less than or equal to " + value);
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Date value) {
		empty = false;
System.out.println(binding + " less than or equal to " + value);
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Integer value) {
		empty = false;
System.out.println(binding + " less than or equal to " + value);
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Long value) {
		empty = false;
System.out.println(binding + " less than or equal to " + value);
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Decimal value) {
		empty = false;
System.out.println(binding + " less than or equal to " + value);
		detailFilter.addLessThanOrEqualTo(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addLessThanOrEqualTo(binding, value);
		}
	}

	@Override
	public void addBetween(String binding, String start, String end) {
		empty = false;
System.out.println(binding + " between " + start + " and " + end);
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
	}

	@Override
	public void addBetween(String binding, Date start, Date end) {
		empty = false;
System.out.println(binding + " between " + start + " and " + end);
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
	}

	@Override
	public void addBetween(String binding, Integer start, Integer end) {
		empty = false;
System.out.println(binding + " between " + start + " and " + end);
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
	}

	@Override
	public void addBetween(String binding, Long start, Long end) {
		empty = false;
System.out.println(binding + " between " + start + " and " + end);
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
	}

	@Override
	public void addBetween(String binding, Decimal start, Decimal end) {
		empty = false;
System.out.println(binding + " between " + start + " and " + end);
		detailFilter.addBetween(binding, start, end);
		if (summaryFilter != null) {
			summaryFilter.addBetween(binding, start, end);
		}
	}

	@Override
	public void addWithin(String binding, Geometry value) {
		empty = false;
		detailFilter.addWithin(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addWithin(binding, value);
		}
	}

	@Override
	public void addContains(String binding, Geometry value) {
		empty = false;
		detailFilter.addContains(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addContains(binding, value);
		}
	}

	@Override
	public void addCrosses(String binding, Geometry value) {
		empty = false;
		detailFilter.addCrosses(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addCrosses(binding, value);
		}
	}

	@Override
	public void addDisjoint(String binding, Geometry value) {
		empty = false;
		detailFilter.addDisjoint(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addDisjoint(binding, value);
		}
	}

	@Override
	public void addIntersects(String binding, Geometry value) {
		empty = false;
		detailFilter.addIntersects(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addIntersects(binding, value);
		}
	}

	@Override
	public void addOverlaps(String binding, Geometry value) {
		empty = false;
		detailFilter.addOverlaps(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addOverlaps(binding, value);
		}
	}

	@Override
	public void addTouches(String binding, Geometry value) {
		empty = false;
		detailFilter.addTouches(binding, value);
		if (summaryFilter != null) {
			summaryFilter.addTouches(binding, value);
		}
	}

	@Override
	public boolean isEmpty() {
		return empty;
	}
}

package org.skyve.metadata.view.model.list;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.Predicate;
import org.apache.commons.collections.PredicateUtils;
import org.skyve.domain.Bean;
import org.skyve.domain.types.Decimal;
import org.skyve.util.Binder;
import org.skyve.wildcat.domain.MapBean;

import com.vividsolutions.jts.geom.Geometry;

public class InMemoryFilter implements Filter {
	private List<Predicate> predicates = new ArrayList<>();
	
	private abstract static class MyPredicate<T extends Object> implements Predicate {
		private String binding;
		private String operatorDescription;
		private T value;
		private T start;
		private T end;
		MyPredicate(String binding, String operatorDescription, T value, T start, T end) {
			this.binding = binding;
			this.operatorDescription = operatorDescription;
			this.value = value;
			this.start = start;
			this.end = end;
		}
		
		@SuppressWarnings("hiding")
		abstract boolean evaluate(Object bean, String binding, T value, T start, T end) throws Exception;
		
		@Override
		public final boolean evaluate(Object object) {
			try {
				return evaluate(object, binding, value, start, end);
			}
			catch (Exception e) {
				return false;
			}
		}
		
		@Override
		public String toString() {
			StringBuilder result = new StringBuilder(128);

			result.append(binding).append(' ').append(operatorDescription);
			if (value != null) {
				result.append(' ').append(value);
			}
			else if ((start != null) || (end != null)) {
				result.append(' ').append(start).append(" and ").append(end);
			}

			return result.toString();
		}
	}
	
	@Override
	public void addAnd(Filter filter) {
		predicates.add(PredicateUtils.allPredicate(((InMemoryFilter) filter).predicates));
	}

	@Override
	public void addOr(Filter filter) {
		// make a new predicate with the existing predicates and'd, unless there is only 1
		// ie make 2 arguments and or them together
		Predicate predicate1 = PredicateUtils.allPredicate(predicates);
		Predicate predicate2 = PredicateUtils.allPredicate(((InMemoryFilter) filter).predicates);
		predicates.clear();
		predicates.add(PredicateUtils.orPredicate(predicate1, predicate2));
	}

	@Override
	public void addTagged(String tagId, boolean tagged) {
		// TODO Auto-generated method stub
	}

	@Override
	public void addNull(final String binding) {
		predicates.add(new MyPredicate<Object>(binding, "is null", null, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Object value, Object start, Object end) throws Exception {
				return (Binder.get(bean, binding) == null);
			}
		});
	}

	@Override
	public void addNotNull(String binding) {
		predicates.add(new MyPredicate<Object>(binding, "is not null", null, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Object value, Object start, Object end) throws Exception {
				return (Binder.get(bean, binding) != null);
			}
		});
	}

	@Override
	public void addEquals(String binding, final String value) {
		predicates.add(new MyPredicate<String>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				return (value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addEquals(String binding, Date value) {
		predicates.add(new MyPredicate<Date>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Date value, Date start, Date end) throws Exception {
				return (value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addEquals(String binding, Integer value) {
		predicates.add(new MyPredicate<Integer>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Integer value, Integer start, Integer end) throws Exception {
				return (value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addEquals(String binding, Long value) {
		predicates.add(new MyPredicate<Long>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Long value, Long start, Long end) throws Exception {
				return (value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addEquals(String binding, Decimal value) {
		predicates.add(new MyPredicate<Decimal>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Decimal value, Decimal start, Decimal end) throws Exception {
				return (value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addEquals(String binding, Boolean value) {
		predicates.add(new MyPredicate<Boolean>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Boolean value, Boolean start, Boolean end) throws Exception {
				return (value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addEquals(String binding, Enum<?> value) {
		predicates.add(new MyPredicate<Enum<?>>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Enum<?> value, Enum<?> start, Enum<?> end) throws Exception {
				return (value.equals(Binder.convert(value.getClass(), Binder.get(bean, binding))));
			}
		});
	}

	@Override
	public void addEquals(String binding, Geometry value) {
		predicates.add(new MyPredicate<Geometry>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Geometry value, Geometry start, Geometry end) throws Exception {
				return (value.equals(Binder.convert(Geometry.class, Binder.get(bean, binding))));
			}
		});
	}

	@Override
	public void addNotEquals(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				return (! value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEquals(String binding, Date value) {
		predicates.add(new MyPredicate<Date>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Date value, Date start, Date end) throws Exception {
				return (! value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEquals(String binding, Integer value) {
		predicates.add(new MyPredicate<Integer>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Integer value, Integer start, Integer end) throws Exception {
				return (! value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEquals(String binding, Long value) {
		predicates.add(new MyPredicate<Long>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Long value, Long start, Long end) throws Exception {
				return (! value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEquals(String binding, Decimal value) {
		predicates.add(new MyPredicate<Decimal>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Decimal value, Decimal start, Decimal end) throws Exception {
				return (! value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEquals(String binding, Boolean value) {
		predicates.add(new MyPredicate<Boolean>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Boolean value, Boolean start, Boolean end) throws Exception {
				return (! value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEquals(String binding, Enum<?> value) {
		predicates.add(new MyPredicate<Enum<?>>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Enum<?> value, Enum<?> start, Enum<?> end) throws Exception {
				return (! value.equals(Binder.convert(value.getClass(), Binder.get(bean, binding))));
			}
		});
	}

	@Override
	public void addNotEquals(String binding, Geometry value) {
		predicates.add(new MyPredicate<Geometry>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Geometry value, Geometry start, Geometry end) throws Exception {
				return (! value.equals(Binder.convert(Geometry.class, Binder.get(bean, binding))));
			}
		});
	}

	@Override
	public void addEqualsIgnoreCase(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding, "equalsIgnoreCase", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				return (value.equalsIgnoreCase((String) Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEqualsIgnoreCase(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding, "! equalsIgnoreCase", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				return (! value.equalsIgnoreCase((String) Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addContains(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding, "contains", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.toUpperCase().contains(value.toUpperCase()));
			}
		});
	}

	@Override
	public void addNotContains(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding, "! contains", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue == null) || (! beanValue.toUpperCase().contains(value.toUpperCase())));
			}
		});
	}

	@Override
	public void addStartsWith(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding, "startsWith", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.toUpperCase().startsWith(value.toUpperCase()));
			}
		});
	}

	@Override
	public void addNotStartsWith(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding, "! startsWith", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue == null) || (! beanValue.toUpperCase().startsWith(value.toUpperCase())));
			}
		});
	}

	@Override
	public void addEndsWith(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding, "endsWith", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.toUpperCase().endsWith(value.toUpperCase()));
			}
		});
	}

	@Override
	public void addNotEndsWith(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding, "! endsWith", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue == null) || (! beanValue.toUpperCase().endsWith(value.toUpperCase())));
			}
		});
	}

	@Override
	public void addGreaterThan(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding, ">", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) > 0);
			}
		});
	}

	@Override
	public void addGreaterThan(String binding, Date value) {
		predicates.add(new MyPredicate<Date>(binding, ">", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Date value, Date start, Date end) throws Exception {
				Date beanValue = (Date) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) > 0);
			}
		});
	}

	@Override
	public void addGreaterThan(String binding, Integer value) {
		predicates.add(new MyPredicate<Integer>(binding,  ">", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Integer value, Integer start, Integer end) throws Exception {
				Integer beanValue = (Integer) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) > 0);
			}
		});
	}

	@Override
	public void addGreaterThan(String binding, Long value) {
		predicates.add(new MyPredicate<Long>(binding,  ">", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Long value, Long start, Long end) throws Exception {
				Long beanValue = (Long) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) > 0);
			}
		});
	}

	@Override
	public void addGreaterThan(String binding, Decimal value) {
		predicates.add(new MyPredicate<Decimal>(binding,  ">", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Decimal value, Decimal start, Decimal end) throws Exception {
				Decimal beanValue = (Decimal) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) > 0);
			}
		});
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding,  ">=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) >= 0);
			}
		});
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Date value) {
		predicates.add(new MyPredicate<Date>(binding, ">=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Date value, Date start, Date end) throws Exception {
				Date beanValue = (Date) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) >= 0);
			}
		});
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Integer value) {
		predicates.add(new MyPredicate<Integer>(binding, ">=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Integer value, Integer start, Integer end) throws Exception {
				Integer beanValue = (Integer) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) >= 0);
			}
		});
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Long value) {
		predicates.add(new MyPredicate<Long>(binding, ">=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Long value, Long start, Long end) throws Exception {
				Long beanValue = (Long) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) >= 0);
			}
		});
	}

	@Override
	public void addGreaterThanOrEqualTo(String binding, Decimal value) {
		predicates.add(new MyPredicate<Decimal>(binding, ">=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Decimal value, Decimal start, Decimal end) throws Exception {
				Decimal beanValue = (Decimal) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) >= 0);
			}
		});
	}

	@Override
	public void addLessThan(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding, "<", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) < 0);
			}
		});
	}

	@Override
	public void addLessThan(String binding, Date value) {
		predicates.add(new MyPredicate<Date>(binding, "<", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Date value, Date start, Date end) throws Exception {
				Date beanValue = (Date) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) < 0);
			}
		});
	}

	@Override
	public void addLessThan(String binding, Integer value) {
		predicates.add(new MyPredicate<Integer>(binding, "<", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Integer value, Integer start, Integer end) throws Exception {
				Integer beanValue = (Integer) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) < 0);
			}
		});
	}

	@Override
	public void addLessThan(String binding, Long value) {
		predicates.add(new MyPredicate<Long>(binding, "<", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Long value, Long start, Long end) throws Exception {
				Long beanValue = (Long) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) < 0);
			}
		});
	}

	@Override
	public void addLessThan(String binding, Decimal value) {
		predicates.add(new MyPredicate<Decimal>(binding, "<", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Decimal value, Decimal start, Decimal end) throws Exception {
				Decimal beanValue = (Decimal) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) < 0);
			}
		});
	}

	@Override
	public void addLessThanOrEqualTo(String binding, String value) {
		predicates.add(new MyPredicate<String>(binding, "<=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) <= 0);
			}
		});
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Date value) {
		predicates.add(new MyPredicate<Date>(binding, "<=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Date value, Date start, Date end) throws Exception {
				Date beanValue = (Date) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) <= 0);
			}
		});
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Integer value) {
		predicates.add(new MyPredicate<Integer>(binding, "<=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Integer value, Integer start, Integer end) throws Exception {
				Integer beanValue = (Integer) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) <= 0);
			}
		});
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Long value) {
		predicates.add(new MyPredicate<Long>(binding, "<=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Long value, Long start, Long end) throws Exception {
				Long beanValue = (Long) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) <= 0);
			}
		});
	}

	@Override
	public void addLessThanOrEqualTo(String binding, Decimal value) {
		predicates.add(new MyPredicate<Decimal>(binding, "<=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Decimal value, Decimal start, Decimal end) throws Exception {
				Decimal beanValue = (Decimal) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) <= 0);
			}
		});
	}

	@Override
	public void addBetween(String binding, String start, String end) {
		predicates.add(new MyPredicate<String>(binding, "between", null, start, end) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, String value, String start, String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				beanValue = beanValue.toUpperCase();
				return ((beanValue != null) && (start.toUpperCase().compareTo(beanValue) <= 0) && (end.toUpperCase().compareTo(beanValue) >= 0));
			}
		});
	}

	@Override
	public void addBetween(String binding, Date start, Date end) {
		predicates.add(new MyPredicate<Date>(binding, "between", null, start, end) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Date value, Date start, Date end) throws Exception {
				Date beanValue = (Date) Binder.get(bean, binding);
				return ((beanValue != null) && (start.compareTo(beanValue) <= 0) && (end.compareTo(beanValue) >= 0));
			}
		});
	}

	@Override
	public void addBetween(String binding, Integer start, Integer end) {
		predicates.add(new MyPredicate<Integer>(binding, "between", null, start, end) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Integer value, Integer start, Integer end) throws Exception {
				Integer beanValue = (Integer) Binder.get(bean, binding);
				return ((beanValue != null) && (start.compareTo(beanValue) <= 0) && (end.compareTo(beanValue) >= 0));
			}
		});
	}

	@Override
	public void addBetween(String binding, Long start, Long end) {
		predicates.add(new MyPredicate<Long>(binding, "between", null, start, end) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Long value, Long start, Long end) throws Exception {
				Long beanValue = (Long) Binder.get(bean, binding);
				return ((beanValue != null) && (start.compareTo(beanValue) <= 0) && (end.compareTo(beanValue) >= 0));
			}
		});
	}

	@Override
	public void addBetween(String binding, Decimal start, Decimal end) {
		predicates.add(new MyPredicate<Decimal>(binding, "between", null, start, end) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Decimal value, Decimal start, Decimal end) throws Exception {
				Decimal beanValue = (Decimal) Binder.get(bean, binding);
				return ((beanValue != null) && (start.compareTo(beanValue) <= 0) && (end.compareTo(beanValue) >= 0));
			}
		});
	}

	@Override
	public void addWithin(String binding, Geometry value) {
		predicates.add(new MyPredicate<Geometry>(binding, "within", null, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Geometry value, Geometry start, Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.within(value));
			}
		});
	}

	@Override
	public void addContains(String binding, Geometry value) {
		predicates.add(new MyPredicate<Geometry>(binding, "contains", null, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Geometry value, Geometry start, Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.contains(value));
			}
		});
	}

	@Override
	public void addCrosses(String binding, Geometry value) {
		predicates.add(new MyPredicate<Geometry>(binding, "crosses", null, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Geometry value, Geometry start, Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.crosses(value));
			}
		});
	}

	@Override
	public void addDisjoint(String binding, Geometry value) {
		predicates.add(new MyPredicate<Geometry>(binding, "disjoint", null, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Geometry value, Geometry start, Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.disjoint(value));
			}
		});
	}

	@Override
	public void addIntersects(String binding, Geometry value) {
		predicates.add(new MyPredicate<Geometry>(binding, "intersects", null, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Geometry value, Geometry start, Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.intersects(value));
			}
		});
	}

	@Override
	public void addOverlaps(String binding, Geometry value) {
		predicates.add(new MyPredicate<Geometry>(binding, "overlaps", null, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Geometry value, Geometry start, Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.overlaps(value));
			}
		});
	}

	@Override
	public void addTouches(String binding, Geometry value) {
		predicates.add(new MyPredicate<Geometry>(binding, "touches", null, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(Object bean, String binding, Geometry value, Geometry start, Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.touches(value));
			}
		});
	}

	@Override
	public boolean isEmpty() {
		return predicates.isEmpty();
	}
	
	void filter(List<Bean> rows) {
		CollectionUtils.filter(rows, PredicateUtils.allPredicate(predicates));
	}
	
	public static void main(String[] args) {
		List<Bean> poo = new ArrayList<>(4);

		Map<String, Object> map = new TreeMap<>();
		map.put("name", "Ted");
		MapBean bean = new MapBean("admin", "Contact", map);
		poo.add(bean);

		map = new TreeMap<>();
		map.put("name", "Fred");
		bean = new MapBean("admin", "Contact", map);
		poo.add(bean);

		map = new TreeMap<>();
		map.put("name", "Jed");
		bean = new MapBean("admin", "Contact", map);
		poo.add(bean);

		map = new TreeMap<>();
		map.put("name", "Ned");
		bean = new MapBean("admin", "Contact", map);
		poo.add(bean);

		map = new TreeMap<>();
		map.put("name", null);
		bean = new MapBean("admin", "Contact", map);
		poo.add(bean);


		InMemoryFilter f = new InMemoryFilter();
		f.addNotStartsWith("name", "Je");
		
		f.filter(poo);
		System.out.println(poo);
	}
}

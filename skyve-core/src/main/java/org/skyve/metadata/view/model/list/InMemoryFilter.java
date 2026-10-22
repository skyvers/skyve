package org.skyve.metadata.view.model.list;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.Predicate;
import org.apache.commons.collections.PredicateUtils;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.domain.types.Decimal;
import org.skyve.util.Binder;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class InMemoryFilter implements Filter {
	private static final String BETWEEN_OPERATOR = "between";

	private List<Predicate> predicates = new ArrayList<>();
	
	private abstract static class MyPredicate<T extends Object> implements Predicate {
		private String binding;
		private String operatorDescription;
		private T value;
		private T start;
		private T end;
		MyPredicate(@Nonnull String binding, @Nonnull String operatorDescription, @Nullable T value, @Nullable T start, @Nullable T end) {
			this.binding = binding;
			this.operatorDescription = operatorDescription;
			this.value = value;
			this.start = start;
			this.end = end;
		}
		
		@SuppressWarnings("hiding")
		abstract boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable T value, @Nullable T start, @Nullable T end) throws Exception;
		
		@Override
		public final boolean evaluate(@SuppressWarnings("null") @Nonnull Object object) {
			try {
				return evaluate(object, binding, value, start, end);
			}
			catch (@SuppressWarnings("unused") Exception e) {
				return false;
			}
		}
		
		@Override
		public @Nonnull String toString() {
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
	public void addAnd(@Nonnull Filter filter) {
		predicates.add(PredicateUtils.allPredicate(((InMemoryFilter) filter).predicates));
	}

	@Override
	public void addOr(@Nonnull Filter filter) {
		// make a new predicate with the existing predicates and'd, unless there is only 1
		// ie make 2 arguments and or them together
		Predicate predicate1 = PredicateUtils.allPredicate(predicates);
		Predicate predicate2 = PredicateUtils.allPredicate(((InMemoryFilter) filter).predicates);
		predicates.clear();
		predicates.add(PredicateUtils.orPredicate(predicate1, predicate2));
	}

	@Override
	public void addTagged(@Nonnull String tagId, boolean tagged) {
		// TODO Auto-generated method stub
	}

	@Override
	public void addNull(final @Nonnull String binding) {
		predicates.add(new MyPredicate<>(binding, "is null", null, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Object value, @Nullable Object start, @Nullable Object end) throws Exception {
				return (Binder.get(bean, binding) == null);
			}
		});
	}

	@Override
	public void addNotNull(@Nonnull String binding) {
		predicates.add(new MyPredicate<>(binding, "is not null", null, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Object value, @Nullable Object start, @Nullable Object end) throws Exception {
				return (Binder.get(bean, binding) != null);
			}
		});
	}

	@Override
	public void addEquals(@Nonnull String binding, final @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				return (value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Date value) {
		predicates.add(new MyPredicate<>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Date value, @Nullable Date start, @Nullable Date end) throws Exception {
				return (value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Integer value) {
		predicates.add(new MyPredicate<>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Integer value, @Nullable Integer start, @Nullable Integer end) throws Exception {
				return (value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Long value) {
		predicates.add(new MyPredicate<>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Long value, @Nullable Long start, @Nullable Long end) throws Exception {
				return (value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Decimal value) {
		predicates.add(new MyPredicate<>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Decimal value, @Nullable Decimal start, @Nullable Decimal end) throws Exception {
				return (value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Boolean value) {
		predicates.add(new MyPredicate<>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Boolean value, @Nullable Boolean start, @Nullable Boolean end) throws Exception {
				return (value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Enum<?> value) {
		predicates.add(new MyPredicate<Enum<?>>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Enum<?> value, @Nullable Enum<?> start, @Nullable Enum<?> end) throws Exception {
				return (value.equals(Binder.convert(value.getClass(), Binder.get(bean, binding))));
			}
		});
	}

	@Override
	public void addEquals(@Nonnull String binding, @Nonnull Geometry value) {
		predicates.add(new MyPredicate<>(binding, "=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Geometry value, @Nullable Geometry start, @Nullable Geometry end) throws Exception {
				return (value.equals(Binder.convert(Geometry.class, Binder.get(bean, binding))));
			}
		});
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				return (! value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Date value) {
		predicates.add(new MyPredicate<>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Date value, @Nullable Date start, @Nullable Date end) throws Exception {
				return (! value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Integer value) {
		predicates.add(new MyPredicate<>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Integer value, @Nullable Integer start, @Nullable Integer end) throws Exception {
				return (! value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Long value) {
		predicates.add(new MyPredicate<>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Long value, @Nullable Long start, @Nullable Long end) throws Exception {
				return (! value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Decimal value) {
		predicates.add(new MyPredicate<>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Decimal value, @Nullable Decimal start, @Nullable Decimal end) throws Exception {
				return (! value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Boolean value) {
		predicates.add(new MyPredicate<>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Boolean value, @Nullable Boolean start, @Nullable Boolean end) throws Exception {
				return (! value.equals(Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Enum<?> value) {
		predicates.add(new MyPredicate<Enum<?>>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Enum<?> value, @Nullable Enum<?> start, @Nullable Enum<?> end) throws Exception {
				return (! value.equals(Binder.convert(value.getClass(), Binder.get(bean, binding))));
			}
		});
	}

	@Override
	public void addNotEquals(@Nonnull String binding, @Nonnull Geometry value) {
		predicates.add(new MyPredicate<>(binding, "!=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Geometry value, @Nullable Geometry start, @Nullable Geometry end) throws Exception {
				return (! value.equals(Binder.convert(Geometry.class, Binder.get(bean, binding))));
			}
		});
	}

	@Override
	public void addEqualsIgnoreCase(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, "equalsIgnoreCase", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				return (value.equalsIgnoreCase((String) Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addNotEqualsIgnoreCase(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, "! equalsIgnoreCase", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				return (! value.equalsIgnoreCase((String) Binder.get(bean, binding)));
			}
		});
	}

	@Override
	public void addContains(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, "contains", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.toUpperCase().contains(value.toUpperCase()));
			}
		});
	}

	@Override
	public void addNotContains(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, "! contains", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue == null) || (! beanValue.toUpperCase().contains(value.toUpperCase())));
			}
		});
	}

	@Override
	public void addStartsWith(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, "startsWith", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.toUpperCase().startsWith(value.toUpperCase()));
			}
		});
	}

	@Override
	public void addNotStartsWith(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, "! startsWith", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue == null) || (! beanValue.toUpperCase().startsWith(value.toUpperCase())));
			}
		});
	}

	@Override
	public void addEndsWith(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, "endsWith", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.toUpperCase().endsWith(value.toUpperCase()));
			}
		});
	}

	@Override
	public void addNotEndsWith(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, "! endsWith", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue == null) || (! beanValue.toUpperCase().endsWith(value.toUpperCase())));
			}
		});
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, ">", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) > 0);
			}
		});
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull Date value) {
		predicates.add(new MyPredicate<>(binding, ">", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Date value, @Nullable Date start, @Nullable Date end) throws Exception {
				Date beanValue = (Date) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) > 0);
			}
		});
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull Integer value) {
		predicates.add(new MyPredicate<>(binding,  ">", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Integer value, @Nullable Integer start, @Nullable Integer end) throws Exception {
				Integer beanValue = (Integer) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) > 0);
			}
		});
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull Long value) {
		predicates.add(new MyPredicate<>(binding,  ">", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Long value, @Nullable Long start, @Nullable Long end) throws Exception {
				Long beanValue = (Long) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) > 0);
			}
		});
	}

	@Override
	public void addGreaterThan(@Nonnull String binding, @Nonnull Decimal value) {
		predicates.add(new MyPredicate<>(binding,  ">", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Decimal value, @Nullable Decimal start, @Nullable Decimal end) throws Exception {
				Decimal beanValue = (Decimal) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) > 0);
			}
		});
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding,  ">=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) >= 0);
			}
		});
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Date value) {
		predicates.add(new MyPredicate<>(binding, ">=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Date value, @Nullable Date start, @Nullable Date end) throws Exception {
				Date beanValue = (Date) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) >= 0);
			}
		});
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Integer value) {
		predicates.add(new MyPredicate<>(binding, ">=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Integer value, @Nullable Integer start, @Nullable Integer end) throws Exception {
				Integer beanValue = (Integer) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) >= 0);
			}
		});
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Long value) {
		predicates.add(new MyPredicate<>(binding, ">=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Long value, @Nullable Long start, @Nullable Long end) throws Exception {
				Long beanValue = (Long) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) >= 0);
			}
		});
	}

	@Override
	public void addGreaterThanOrEqualTo(@Nonnull String binding, @Nonnull Decimal value) {
		predicates.add(new MyPredicate<>(binding, ">=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Decimal value, @Nullable Decimal start, @Nullable Decimal end) throws Exception {
				Decimal beanValue = (Decimal) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) >= 0);
			}
		});
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, "<", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) < 0);
			}
		});
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull Date value) {
		predicates.add(new MyPredicate<>(binding, "<", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Date value, @Nullable Date start, @Nullable Date end) throws Exception {
				Date beanValue = (Date) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) < 0);
			}
		});
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull Integer value) {
		predicates.add(new MyPredicate<>(binding, "<", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Integer value, @Nullable Integer start, @Nullable Integer end) throws Exception {
				Integer beanValue = (Integer) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) < 0);
			}
		});
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull Long value) {
		predicates.add(new MyPredicate<>(binding, "<", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Long value, @Nullable Long start, @Nullable Long end) throws Exception {
				Long beanValue = (Long) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) < 0);
			}
		});
	}

	@Override
	public void addLessThan(@Nonnull String binding, @Nonnull Decimal value) {
		predicates.add(new MyPredicate<>(binding, "<", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Decimal value, @Nullable Decimal start, @Nullable Decimal end) throws Exception {
				Decimal beanValue = (Decimal) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) < 0);
			}
		});
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull String value) {
		predicates.add(new MyPredicate<>(binding, "<=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) <= 0);
			}
		});
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Date value) {
		predicates.add(new MyPredicate<>(binding, "<=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Date value, @Nullable Date start, @Nullable Date end) throws Exception {
				Date beanValue = (Date) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) <= 0);
			}
		});
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Integer value) {
		predicates.add(new MyPredicate<>(binding, "<=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Integer value, @Nullable Integer start, @Nullable Integer end) throws Exception {
				Integer beanValue = (Integer) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) <= 0);
			}
		});
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Long value) {
		predicates.add(new MyPredicate<>(binding, "<=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Long value, @Nullable Long start, @Nullable Long end) throws Exception {
				Long beanValue = (Long) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) <= 0);
			}
		});
	}

	@Override
	public void addLessThanOrEqualTo(@Nonnull String binding, @Nonnull Decimal value) {
		predicates.add(new MyPredicate<>(binding, "<=", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Decimal value, @Nullable Decimal start, @Nullable Decimal end) throws Exception {
				Decimal beanValue = (Decimal) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.compareTo(value) <= 0);
			}
		});
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull String start, @Nonnull String end) {
		predicates.add(new MyPredicate<>(binding, BETWEEN_OPERATOR, null, start, end) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable String value, @Nullable String start, @Nullable String end) throws Exception {
				String beanValue = (String) Binder.get(bean, binding);
				if (beanValue == null) {
					return false;
				}
				beanValue = beanValue.toUpperCase();
				return (start.toUpperCase().compareTo(beanValue) <= 0) && (end.toUpperCase().compareTo(beanValue) >= 0);
			}
		});
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull Date start, @Nonnull Date end) {
		predicates.add(new MyPredicate<>(binding, BETWEEN_OPERATOR, null, start, end) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Date value, @Nullable Date start, @Nullable Date end) throws Exception {
				Date beanValue = (Date) Binder.get(bean, binding);
				return ((beanValue != null) && (start.compareTo(beanValue) <= 0) && (end.compareTo(beanValue) >= 0));
			}
		});
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull Integer start, @Nonnull Integer end) {
		predicates.add(new MyPredicate<>(binding, BETWEEN_OPERATOR, null, start, end) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Integer value, @Nullable Integer start, @Nullable Integer end) throws Exception {
				Integer beanValue = (Integer) Binder.get(bean, binding);
				return ((beanValue != null) && (start.compareTo(beanValue) <= 0) && (end.compareTo(beanValue) >= 0));
			}
		});
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull Long start, @Nonnull Long end) {
		predicates.add(new MyPredicate<>(binding, BETWEEN_OPERATOR, null, start, end) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Long value, @Nullable Long start, @Nullable Long end) throws Exception {
				Long beanValue = (Long) Binder.get(bean, binding);
				return ((beanValue != null) && (start.compareTo(beanValue) <= 0) && (end.compareTo(beanValue) >= 0));
			}
		});
	}

	@Override
	public void addBetween(@Nonnull String binding, @Nonnull Decimal start, @Nonnull Decimal end) {
		predicates.add(new MyPredicate<>(binding, BETWEEN_OPERATOR, null, start, end) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Decimal value, @Nullable Decimal start, @Nullable Decimal end) throws Exception {
				Decimal beanValue = (Decimal) Binder.get(bean, binding);
				return ((beanValue != null) && (start.compareTo(beanValue) <= 0) && (end.compareTo(beanValue) >= 0));
			}
		});
	}

	@Override
	public void addIn(@Nonnull String binding, @Nonnull Object... values) {
		predicates.add(new MyPredicate<>(binding, "in", values, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Object[] values, @Nullable Object[] start, @Nullable Object[] end) throws Exception {
				if (values.length > 0) {
					Object beanValue = Binder.get(bean, binding);
					if (beanValue != null) {
						for (Object value : values) {
							if (beanValue.equals(value)) {
								return true;
							}
						}
					}
				}
				return false;
			}
		});
	}

	@Override
	public void addNotIn(@Nonnull String binding, @Nonnull Object... values) {
		predicates.add(new MyPredicate<>(binding, "not in", values, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Object[] values, @Nullable Object[] start, @Nullable Object[] end) throws Exception {
				if (values.length > 0) {
					Object beanValue = Binder.get(bean, binding);
					if (beanValue != null) {
						for (Object value : values) {
							if (beanValue.equals(value)) {
								return false;
							}
						}
					}
				}
				return true;
			}
		});
	}

	@Override
	public void addWithin(@Nonnull String binding, @Nonnull Geometry value) {
		predicates.add(new MyPredicate<>(binding, "within", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Geometry value, @Nullable Geometry start, @Nullable Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.within(value));
			}
		});
	}

	@Override
	public void addContains(@Nonnull String binding, @Nonnull Geometry value) {
		predicates.add(new MyPredicate<>(binding, "contains", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Geometry value, @Nullable Geometry start, @Nullable Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.contains(value));
			}
		});
	}

	@Override
	public void addCrosses(@Nonnull String binding, @Nonnull Geometry value) {
		predicates.add(new MyPredicate<>(binding, "crosses", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Geometry value, @Nullable Geometry start, @Nullable Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.crosses(value));
			}
		});
	}

	@Override
	public void addDisjoint(@Nonnull String binding, @Nonnull Geometry value) {
		predicates.add(new MyPredicate<>(binding, "disjoint", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Geometry value, @Nullable Geometry start, @Nullable Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.disjoint(value));
			}
		});
	}

	@Override
	public void addIntersects(@Nonnull String binding, @Nonnull Geometry value) {
		predicates.add(new MyPredicate<>(binding, "intersects", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Geometry value, @Nullable Geometry start, @Nullable Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.intersects(value));
			}
		});
	}

	@Override
	public void addOverlaps(@Nonnull String binding, @Nonnull Geometry value) {
		predicates.add(new MyPredicate<>(binding, "overlaps", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Geometry value, @Nullable Geometry start, @Nullable Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.overlaps(value));
			}
		});
	}

	@Override
	public void addTouches(@Nonnull String binding, @Nonnull Geometry value) {
		predicates.add(new MyPredicate<>(binding, "touches", value, null, null) {
			@Override
			@SuppressWarnings("hiding")
			boolean evaluate(@Nonnull Object bean, @Nonnull String binding, @Nullable Geometry value, @Nullable Geometry start, @Nullable Geometry end) throws Exception {
				Geometry beanValue = (Geometry) Binder.get(bean, binding);
				return ((beanValue != null) && beanValue.touches(value));
			}
		});
	}

	@Override
	public boolean isEmpty() {
		return predicates.isEmpty();
	}
	
	public void filter(@Nonnull List<Bean> rows) {
		CollectionUtils.filter(rows, PredicateUtils.allPredicate(predicates));
	}
}

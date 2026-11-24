package modules.kitchensink.RerenderFixture;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import jakarta.annotation.Nonnull;
import modules.kitchensink.domain.RerenderFixture;

/**
 * Counts the rerenders of Week Ending Date per fixture, outside the conversation.
 * <p>
 * The conversation keeps the changes of the request that finished last, so a count kept in the bean would read 1 after
 * two rerenders sent at once. This count is kept in the server's memory, keyed by the fixture's bizId, and lasts as
 * long as the server runs (an entry is a few bytes, and only tests make many fixtures).
 */
public class RerenderFixtureBizlet extends Bizlet<RerenderFixture> {
	private static final Map<String, AtomicInteger> RERENDERS = new ConcurrentHashMap<>();

	/**
	 * Counts a rerender of Week Ending Date.
	 *
	 * @param source	the binding of the widget whose event rerenders the view
	 * @param bean	the fixture
	 * @param webContext	the web context
	 * @throws Exception	if the base class's processing fails
	 */
	@Override
	public void preRerender(@Nonnull String source, @Nonnull RerenderFixture bean, @Nonnull WebContext webContext)
	throws Exception {
		if (RerenderFixture.weekEndingDatePropertyName.equals(source)) {
			RERENDERS.computeIfAbsent(bean.getBizId(), k -> new AtomicInteger()).incrementAndGet();
		}
		super.preRerender(source, bean, webContext);
	}

	/**
	 * Returns how many rerenders of Week Ending Date the server was sent for a fixture.
	 *
	 * @param bizId	the fixture's bizId
	 * @return	the count, 0 if there were none
	 */
	public static int rerenders(@Nonnull String bizId) {
		AtomicInteger count = RERENDERS.get(bizId);
		return (count == null) ? 0 : count.get();
	}
}

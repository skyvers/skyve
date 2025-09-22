package modules.kitchensink;

import java.util.Date;
import java.util.concurrent.TimeUnit;

import org.skyve.EXT;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.job.CancellableJob;
import org.skyve.util.PushMessage;

/**
 * This job sends a bunch of growls; some to the current user, and some to all users.
 */
public class GrowlSpamJob extends CancellableJob {

	final static int howMany = 20;

	@Override
	public void execute() throws InterruptedException {

		// User testUser = CORE.getRepository().retrieveUser("test");

		while (howMany > 0) {

			if (isCancelled()) {
				break;
			}

			{
				PushMessage pm = new PushMessage()
						.user()
						.growl(MessageSeverity.info, "a message for the current user " + new Date());

				EXT.push(pm);
				sleep();
			}

			// {
			//
			// PushMessage pm = new PushMessage()
			// .user(testUser)
			// .growl(MessageSeverity.info, "a message for 'test' " + new Date());
			//
			// EXT.push(pm);
			// sleep();
			// }
			//
			// {
			// PushMessage pm = new PushMessage()
			// .user(testUser)
			// .user()
			// .growl(MessageSeverity.info, "a message for 'test' & 'setup' XXXXX! ");
			//
			// EXT.push(pm);
			// sleep();
			// }

			{

				PushMessage pm = new PushMessage()
						.growl(MessageSeverity.info, "a broadcast message " + new Date());

				EXT.push(pm);
				sleep();
			}

		}

		LOGGER.info("Done");
	}

	private void sleep() throws InterruptedException {
		TimeUnit.SECONDS.sleep(3);
	}

}

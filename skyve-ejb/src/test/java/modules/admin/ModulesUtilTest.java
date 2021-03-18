package modules.admin;

import modules.admin.Group.GroupExtension;
import modules.admin.User.UserExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Group;
import modules.admin.domain.User;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture;
import util.AbstractH2Test;
import util.AbstractH2TestForJUnit5;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import static org.junit.jupiter.api.Assertions.*;

public class ModulesUtilTest extends AbstractH2TestForJUnit5
{

    private DataBuilder db;
    private Contact contact = null;

    private static final Logger logger = Logger.getLogger(ModulesUtilTest.class.getName());
    @BeforeEach
    void setUpTests()
    {
        db = new DataBuilder().fixture(SkyveFixture.FixtureType.crud);
    }

    @AfterEach
    void tearDownTeats()
    {
    }

    @Test
    void createAdminUserFromContactWithGroupShouldFailIfNoContact()
    {
        //given
        contact = null;

        //when
        Executable executable = () -> ModulesUtil.createAdminUserFromContactWithGroup(contact, "groupName", "homeModuleName",false );

        //then
        assertThrows(ValidationException.class, executable);

    }

    @Test
    void createAdminUserFromContactWithGroupShouldFailIfNoGroupName()
    {
        //given
        Contact contact = Contact.newInstance();

        //when
        Executable executable = () -> ModulesUtil.createAdminUserFromContactWithGroup(contact, null, "homeModuleName",false );

        //then
        assertThrows(ValidationException.class, executable);

    }

    
}

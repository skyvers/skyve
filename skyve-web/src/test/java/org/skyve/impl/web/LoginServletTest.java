package org.skyve.impl.web;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.customer.LoginResources;
import org.skyve.metadata.repository.Repository;
import org.skyve.metadata.user.User;
import org.skyve.web.WebContext;

import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;

@Disabled("Until byte buddy can be upl.ofted to allow mockito-inline lib to work")
@ExtendWith(MockitoExtension.class)
class LoginServletTest {

    @Mock
    private HttpServletRequest request;
    @Mock
    private HttpServletResponse response;
    @Mock
    private HttpSession session;
    @Mock
    private RequestDispatcher requestDispatcher;
    @Mock
    private Repository repository;
    @Mock
    private Customer customer;
    @Mock
    private User user;
    @Mock
    private LoginResources loginResources;

    private LoginServlet servlet;
    private MockedStatic<WebUtil> webUtilMock;
    private MockedStatic<CORE> coreMock;

    @BeforeEach
    void setUp() {
        servlet = new LoginServlet();
        when(request.getRequestDispatcher(anyString())).thenReturn(requestDispatcher);
        webUtilMock = mockStatic(WebUtil.class);
        coreMock = mockStatic(CORE.class);
        coreMock.when(CORE::getRepository).thenReturn(repository);
    }

    @AfterEach
    void tearDown() {
        if (webUtilMock != null) {
            webUtilMock.close();
        }
        if (coreMock != null) {
            coreMock.close();
        }
    }

    @Test
    void testLoginPageWithNoSession() throws ServletException, IOException {
        // Setup
        when(request.getSession(false)).thenReturn(null);
        when(request.getServletPath()).thenReturn(LoginServlet.LOGIN_PATH);
        webUtilMock.when(() -> WebUtil.determineCustomerWithoutSession(request)).thenReturn("testCustomer");
        when(repository.getCustomer("testCustomer")).thenReturn(customer);
        when(customer.getLoginResources()).thenReturn(loginResources);
        when(loginResources.getLoginPageURL()).thenReturn(null);

        // Execute
        servlet.doGet(request, response);

        // Verify
        verify(requestDispatcher).forward(request, response);
        verify(request).getRequestDispatcher("/pages/login.jsp");
    }

    @Test
    void testLoginPageWithExistingSession() throws ServletException, IOException {
        // Setup
        when(request.getSession(false)).thenReturn(session);
        when(session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME)).thenReturn(user);
        when(user.getCustomer()).thenReturn(customer);
        when(customer.getName()).thenReturn("testCustomer");
        when(request.getServletPath()).thenReturn(LoginServlet.LOGIN_PATH);
        when(repository.getCustomer("testCustomer")).thenReturn(customer);
        when(customer.getLoginResources()).thenReturn(loginResources);
        when(loginResources.getLoginPageURL()).thenReturn(null);

        // Execute
        servlet.doGet(request, response);

        // Verify
        verify(requestDispatcher).forward(request, response);
        verify(request).getRequestDispatcher("/pages/login.jsp");
    }

    @Test
    void testLoggedOutPageWithNoSession() throws ServletException, IOException {
        // Setup
        when(request.getSession(false)).thenReturn(null);
        when(request.getServletPath()).thenReturn(LoginServlet.LOGGED_OUT_PATH);
        webUtilMock.when(() -> WebUtil.determineCustomerWithoutSession(request)).thenReturn("testCustomer");
        when(repository.getCustomer("testCustomer")).thenReturn(customer);
        when(customer.getLoginResources()).thenReturn(loginResources);
        when(loginResources.getLoggedOutPageURL()).thenReturn(null);

        // Execute
        servlet.doGet(request, response);

        // Verify
        verify(requestDispatcher).forward(request, response);
        verify(request).getRequestDispatcher("/pages/loggedOut.jsp");
    }

    @Test
    void testCustomLoginPage() throws ServletException, IOException {
        // Setup
        when(request.getSession(false)).thenReturn(null);
        when(request.getServletPath()).thenReturn(LoginServlet.LOGIN_PATH);
        webUtilMock.when(() -> WebUtil.determineCustomerWithoutSession(request)).thenReturn("testCustomer");
        when(repository.getCustomer("testCustomer")).thenReturn(customer);
        when(customer.getLoginResources()).thenReturn(loginResources);
        when(loginResources.getLoginPageURL()).thenReturn("/custom/login.jsp");

        // Execute
        servlet.doGet(request, response);

        // Verify
        verify(requestDispatcher).forward(request, response);
        verify(request).getRequestDispatcher("/custom/login.jsp");
    }
} 
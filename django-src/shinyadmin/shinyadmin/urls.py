"""shinyadmin URL Configuration

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/2.2/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  path('', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  path('', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.urls import include, path
    2. Add a URL to urlpatterns:  path('blog/', include('blog.urls'))
"""
from django.contrib import admin
from django.contrib.auth import views as auth_views
from django.urls import path

from django.views.generic import TemplateView

import core.views as core_views
import core.urls as core_urls

urlpatterns = [
    # administration root
    path('admin/', admin.site.urls),

    # landing page
    path('', core_views.landing_page),

    # shiny app wrapper
    path('dams_mcda_wrapper/', core_views.shiny_app_wrapper),

    # user register
    path('register/', core_views.register, name="register"),

    # user login
    path('login/', core_views.login, name="login"),
    # user logout
    path('logout/', core_views.logout, name="logout"),

    # user password reset
    path('password_reset/', core_views.password_reset, name="password_reset"),
    # user password reset done (email sent to user)
    path('password_reset/done/', auth_views.PasswordResetDoneView.as_view(template_name="password_reset_done.html"), name="password_reset_done"),
    # user changes password page
    path('password_reset/confirm/<uidb64>/<token>/', auth_views.PasswordResetConfirmView.as_view(template_name="password_reset_confirm.html"), name="password_reset_confirm"),
    # user password reset complete (post user confirms new password)
    path('password_reset/complete/', auth_views.PasswordResetCompleteView.as_view(template_name="password_reset_complete.html"), name="password_reset_complete"),

    # returns username and session info
    path('api/get_user_session/', core_views.get_user_session, name="get_user_session"),
    # given username and session info validates with django request credentials
    path('api/verify_user_session/', core_views.verify_user_session, name="verify_user_session"),

]

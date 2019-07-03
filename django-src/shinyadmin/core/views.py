from django.shortcuts import render, redirect

from django.http import JsonResponse

from django.contrib.auth import authenticate
from django.contrib.auth import login as auth_login
from django.contrib.auth import logout as auth_logout
from django.contrib.auth.forms import AuthenticationForm, PasswordResetForm

from .forms import RegistrationForm
from . import serializers as core_serializers


def landing_page(request):
    """
        Application Entrypoint
        if user is already logged in redirect to shiny application
        if not send them to the login page which should contain links to
        register if the user doesnt have an account
    """
    if request.user.is_authenticated:
        return redirect('/dams_mcda/')
    else:
        return redirect('/login/')


def logout(request):
    # doesnt throw errors if not logged in
    auth_logout(request)


def login(request):
    if request.method == "POST":
        username = request.POST['username']
        password = request.POST['password']

        user = authenticate(request, username=username, password=password)

        if user is not None:
            auth_login(request, user)
            return redirect('/')
        else:
            form = AuthenticationForm(request.POST)
            return render(request, "login.html", {'form': form, 'errors': 'invalid login credentials, please try again'})
    else:
        form = AuthenticationForm()
        return render(request, "login.html", locals())


def register(request):
    if request.method == "POST":
        form = RegistrationForm(request.POST)
        if form.is_valid():
            form.save()
            return redirect('/login/')
        else:
            return render(request, "register.html", {'form': form})
    else:
        form = RegistrationForm()
        return render(request, "register.html", locals())


def password_reset(request):
    if request.method == "POST":
        form = PasswordResetForm(request.POST)
        if form.is_valid():
            form.save(request=request)
            return redirect('password_reset_done')
        else:
            return render(request, "password_reset.html", {'form': form})
    else:
        form = PasswordResetForm()
        return render(request, "password_reset.html", locals())


def get_user_session(request):
    """
        return information needed access user and session later on
    """
    if request.user.is_authenticated:
        session = request.session.session_key
        user = request.user

        user_data = core_serializers.DamsMCDAUserSerializer(user, many=False)

        return JsonResponse({
            "session": session,
            "user": user_data.data
        })
    else:
        raise Http404("User is not logged in")

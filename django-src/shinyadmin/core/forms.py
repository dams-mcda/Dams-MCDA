from django.contrib.auth.forms import UserCreationForm
from django import forms
from .models import DamsMCDAUser


class RegistrationForm(UserCreationForm):
    # could modify form fields here

    class Meta(UserCreationForm.Meta):
        model = DamsMCDAUser
        fields = UserCreationForm.Meta.fields + ('group',)

